module ProgramGraphGenerator

// open FSharp.Text.Lexing
// open System
// #load "TypesAST.fs"
// open TypesAST
// #load "Parser.fs"
// open Parser
// #load "Lexer.fs"
// open Lexer
// #load "ActionToText.fs"
// open ActionToText
// #load "DotWriter.fs"
// open DotWriter
// #load "DepthFinder.fs"
// open DepthFinder

let mutable freshNodeIndex = 1
let mutable edgeSteps = 0

let incFreshNodeIndex() = freshNodeIndex <- freshNodeIndex + 1
let decFreshNodeIndex() = 
    freshNodeIndex <- freshNodeIndex - 1
    printfn "Node decremented to %d" freshNodeIndex

let negateBooleanExp bexp=
    NotExpr(bexp)

 
let rec getEdgeString edge=
    match edge with
        | (orig, action, dest) -> "(" + string orig + ")" + (printAction action) + "(" + string dest + ")"

let getEdgeTuple edge=
    match edge with
        | (orig, action, dest) -> "(" + string orig + ", " + (printAction action) + ", " + string dest + ")"

let printProgramGraph graph=
    let (graph, _) = graph
    List.iteri (fun i e -> printfn "%s" (getEdgeString e)) graph

let printEdgeTuples list = 
    printf "["
    list |> List.iter (fun e -> printf "%s" (getEdgeTuple e))
    printf "]"
    printfn ""

let updateNodeIndex pg =
    let mutable max = freshNodeIndex - 1
    pg |> List.iter(fun (s, _, e) -> if s > max then max <- s
                                     if e > max then max <- e)
    freshNodeIndex <- max + 1

let joinLists list1 list2 = 
    let a = list1 @ list2
    if edgeSteps = 1 then
        Console.ReadLine() |> ignore
        printEdgeTuples a
    updateNodeIndex a
    a

let getFresh n =
    printfn "get fresh: %d" (freshNodeIndex + n)
    freshNodeIndex + n

let rec generateCommandEdges (e, startNode, endNode, programGraph) =
  match e with
    | Assign(x,y) -> joinLists programGraph [Edge(startNode, Assignment(x, y), endNode)], Set.empty
                
    | Skip  -> joinLists programGraph [Edge(startNode, SkipAction, endNode)], Set.empty

    | CommandSeq(x,y) -> let depthx = getDepth x
                         let fresh = getFresh depthx
                         let a, a1 = generateCommandEdges(x, startNode, fresh, programGraph)
                         let b, b1 = generateCommandEdges(y, fresh, endNode, a)
                         b, Set.union a1 b1

    | IfFi(gc) ->   generateGCEdges (gc, startNode, endNode, programGraph)
                    
    | DoOd(gc) -> let negBoolAct = Boolean(match gc with | GuardedCommand(b,_) -> negateBooleanExp b)
                  let doneGc = [Edge(startNode, negBoolAct, endNode)]
                  let edges, set = generateGCEdges (gc, startNode, startNode, programGraph)
                  edges @ doneGc, Set.union set (Set.empty.Add(startNode))

and generateGCEdges (e, startNode, endNode, programGraph) = 
    match e with
        | GuardedCommand(b, exp) -> let fresh = getFresh 0
                                    let a = joinLists programGraph [Edge(startNode, Boolean(b), fresh)]
                                    generateCommandEdges(exp, fresh, endNode, a)
                                    
        | GuardedCommandSeq(gc1, gc2) -> let edgesGC1, set1 = generateGCEdges(gc1, startNode, endNode, programGraph)                                        
                                         let edges2, set2 = generateGCEdges(gc2, startNode, endNode, edgesGC1)
                                         edges2, Set.union set1 set2
                                         

let getProgramGraph e =
    let lastNode = (getDepth e) + 1
    printfn "Last node will be: %d" lastNode
    let graph = generateCommandEdges (e, 0 , lastNode, [])
    graph 


// input parser
let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = Parser.start Lexer.tokenize lexbuf
    res