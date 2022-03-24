// We need to import a couple of modules, including the generated lexer and parser
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "TypesAST.fs"
open TypesAST
#load "Parser.fs"
open Parser
#load "Lexer.fs"
open Lexer
#load "DotWriter.fsx"
open DotWriter
#load "ActionToText.fs"
open ActionToText
#load "DepthFinder.fs"
open DepthFinder

// module ProgramGraphGenerator

let mutable freshNodeIndex = 1
let mutable continuationNode = 0
let mutable currentNode = 0
let mutable edgeSteps = 0
let mutable lastBranchJoin = -1

//alo implement as stack
let mutable branchStartNode = -1
//let programGraph = []

let incFreshNodeIndex() = freshNodeIndex <- freshNodeIndex + 1
let decFreshNodeIndex() = 
    freshNodeIndex <- freshNodeIndex - 1
    printfn "Node decremented to %d" freshNodeIndex


let getLastElement list =
    List.item ((List.length list) - 1) list

type Mem = (string * int) list * (string * int list) list

// Or use map
let mem = Mem([("x", 5); ("y", 5)], [])

let rec getEdgeString edge=
    match edge with
        | Edge(orig, action, dest) -> "(" + string orig + ")" + (printAction action) + "(" + string dest + ")"

let getEdgeTuple edge=
    match edge with
        | Edge(orig, action, dest) -> "(" + string orig + ", " + (printAction action) + ", " + string dest + ")"

let printProgramGraph graph =
    List.iteri (fun i e -> printfn "%s" (getEdgeString e)) graph

let printVars vars = 
    let mutable s = ""
    List.iteri(fun i (id, value) -> s <- s + id + "=" + string value + ", ") vars
    printfn "Node %d: %s" currentNode s

let printEdgeTuples list = 
    printf "["
    list |> List.iter (fun e -> printf "%s" (getEdgeTuple e))
    printf "]"
    printfn ""

let joinLists list1 list2 = 
    let a = list1 @ list2
    if edgeSteps = 1 then
        Console.ReadLine() |> ignore
        printEdgeTuples a
    a

let updateEdge edge newEnd =
    match edge with
        | Edge(s, act, e) -> Edge(s, act, newEnd)



let performCalc action vars =
    match action with
        | Assignment(var, expr) ->  let varText = getTextAri var
                                    let result = getAexprValue expr vars 
                                    vars |> List.mapi (fun i (id, value) -> if varText = id then (varText, result) else (id, value))
                                    
        | SkipAction -> vars


let getFresh n =
    freshNodeIndex + n

let updateNodeIndex pg =
    let mutable max = freshNodeIndex
    pg |> List.iter(fun (Edge(s, _, e)) -> if s > max then max <- s
                                           if e > max then max <- e)
    freshNodeIndex <- max + 1

let rec generateCommandEdges (e, startNode, endNode, programGraph) =
  match e with
    | Assign(x,y) ->let pg = joinLists programGraph [Edge(startNode, Assignment(x, y), endNode)]
                    updateNodeIndex pg
                    pg

    | Skip  ->  let pg = joinLists programGraph [Edge(startNode, SkipAction, endNode)]
                updateNodeIndex pg
                pg

    | CommandSeq(x,y) -> let depthx = getDepth x
                         let fresh = getFresh depthx
                         let a = generateCommandEdges(x, startNode, fresh, programGraph)
                         
                         generateCommandEdges(y, fresh, endNode, a)

    | IfFi(gc) ->   generateGCEdges (gc, startNode, endNode, programGraph)
                    
    | DoOd(gc) -> let doneGc = [Edge(startNode, SkipAction, endNode)]
                  let edges = generateGCEdges (gc, startNode, startNode, programGraph)
                  edges @ doneGc

and generateGCEdges (e, startNode, endNode, programGraph) = 
    match e with
        | GuardedCommand(b, exp) -> let fresh = getFresh 0
                                    let a = joinLists programGraph [Edge(startNode, Boolean(b), fresh)]
                                    updateNodeIndex a 
                                    let b = generateCommandEdges(exp, fresh, endNode, a)
                                    updateNodeIndex b
                                    b
                                    
        | GuardedCommandSeq(gc1, gc2) -> let edgesGC1 = generateGCEdges(gc1, startNode, endNode, programGraph)                                        
                                         generateGCEdges(gc2, startNode, endNode, edgesGC1)
                                         

let getProgramGraph e =
    let lastNode = getDepth e
    printfn "Last node will be: %d" lastNode
    let graph = generateCommandEdges (e, 0 , lastNode, [])
    graph 


let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = Parser.start Lexer.tokenize lexbuf
    res
        

let compileFromFile n =
    let e = parse(System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\GCLExamples\simple.txt"))
    printf "Do you want to see edge steps (0:no, 1:yes): "
    edgeSteps <- int (Console.ReadLine())
    let g = getProgramGraph e
    printProgramGraph g
    DotWriter.writeProgramGraph g
    // getDepth e |> ignore
    //let a = stepExecute nodes vars
    printfn ""

compileFromFile 0
