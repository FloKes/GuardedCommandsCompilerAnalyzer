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

// module ProgramGraphGenerator

let mutable nodeIndex = 0
let mutable continuationNode = 0
let mutable currentNode = 0

//alo implement as stack
let mutable branchStartNode = -1
//let programGraph = []

let incNodeIndex() = nodeIndex <- nodeIndex + 1

type Mem = (string * int) list * (string * int list) list

// Or use map
let mem = Mem([("x", 5); ("y", 5)], [])

let rec getEdgeString edge=
    match edge with
        | Edge(orig, action, dest) -> "(" + string orig + ")" + (printAction action) + "(" + string dest + ")"

let printProgramGraph graph =
    List.iteri (fun i e -> printfn "%s" (getEdgeString e)) graph

let rec getAexprValue e vars =
  match e with
    | Num(x) -> x
    | Identifier(x) -> let mutable r = 0 // If i don't find x, we return 0, which should be
                       List.iter(fun (id, value) -> if id = x then r <- value else failwith "unkown raviable") vars
                       r
    | IdentifierArray(x, y) -> 1
    | TimesExpr(x,y) -> getAexprValue x vars  * getAexprValue y vars
    | DivExpr(x,y) -> getAexprValue x vars  / getAexprValue y vars
    | PlusExpr(x,y) -> getAexprValue x vars  + getAexprValue y vars
    | MinusExpr(x,y) -> getAexprValue x vars  - getAexprValue y vars
    | PowExpr(x,y) -> 2
    | UPlusExpr(x) -> getAexprValue x vars 
    | UMinusExpr(x) -> - getAexprValue x vars 



let printVars vars = 
    let mutable s = ""
    List.iteri(fun i (id, value) -> s <- s + id + "=" + string value + ", ") vars
    printfn "Node %d: %s" currentNode s

let performCalc action vars =
    match action with
        | Assignment(var, expr) ->  let varText = getTextAri var
                                    let result = getAexprValue expr vars 
                                    vars |> List.mapi (fun i (id, value) -> if varText = id then (varText, result) else (id, value))
                                    
        | SkipAction -> vars

let getLastElement list =
    List.item ((List.length list) - 1) list

// let removeLastElement list =


let updateEdge edge newEnd =
    match edge with
        | Edge(s, act, e) -> Edge(s, act, newEnd)



let changeLastNodes (list, lastNode, joinNode) =
    nodeIndex <- nodeIndex - 1
    list |> List.mapi (fun i v -> match v with 
                                        | Edge(s, act, e) -> if e = lastNode then updateEdge v joinNode
                                                                             else v)

let rec generateCommandEdges (e, startNode, endNode, programGraph, startBranch, endBranch) =
  match e with
    | Assign(x,y) -> incNodeIndex()
                     continuationNode <- endNode
                     programGraph @ [Edge(startNode, Assignment(x, y), endNode)]
    | Skip  ->  incNodeIndex()
                continuationNode <- endNode
                programGraph @ [Edge(startNode, SkipAction, endNode)]

    | CommandSeq(x,y) -> let a = generateCommandEdges(x, continuationNode, nodeIndex + 1, programGraph, startBranch, endBranch)
                         generateCommandEdges(y, continuationNode, nodeIndex + 1, a, startBranch, endBranch)

    | IfFi(x) -> let startBranch = startNode
                 branchStartNode <- startBranch
                 // make generate GCEdges return where stuff needs to be merged, and then do it
                 let a = generateGCEdges (x, startNode, endNode, programGraph, startBranch, -1)
                 a

and generateGCEdges (e, startNode, endNode, programGraph, startBranch, endBranch) = 
    match e with
        | GuardedCommand(b, exp) -> incNodeIndex()
                                    continuationNode <- endNode
                                    let a = programGraph @ [Edge(startNode, Boolean(b), endNode)]
                                    let b = generateCommandEdges(exp, continuationNode, nodeIndex + 1, a, startBranch, endBranch)
                                    // if endBranch = -1  
                                    // then b
                                    // else changeLastNodes (b, nodeIndex, endBranch)
                                    b
                                    
        | GuardedCommandSeq(fst, snd) -> let a = generateGCEdges(fst, branchStartNode, endNode, programGraph, startBranch, endBranch)
                                         let endBranch = nodeIndex
                                         generateGCBranchEdges(snd, branchStartNode, nodeIndex + 1, a, startBranch, endBranch)
                                         


and generateGCBranchEdges (e, startNode, endNode, programGraph, startBranch, endBranch) =
        match e with
        | GuardedCommand(x) ->  generateGCEdges (e, startNode, endNode, programGraph, startBranch, endBranch)                          
        | GuardedCommandSeq(fst, snd) -> let a = generateGCEdges(fst, branchStartNode, endNode, programGraph, startBranch, endBranch)
                                         generateGCBranchEdges(snd, branchStartNode, nodeIndex + 1, a, startBranch, endBranch)

let getProgramGraph e =
    let graph = generateCommandEdges (e, 0 , 1, [], -1, -1)
    printProgramGraph graph
    graph 


let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = Parser.start Lexer.tokenize lexbuf

    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that Compiles the program in the text file
let compileFromFile n =
    // We parse the input string
    let e = parse(System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\GCLExamples\simple.txt"))
    let g = getProgramGraph e
    DotWriter.writeProgramGraph g

    //let a = stepExecute nodes vars
    printfn ""

compileFromFile 0
