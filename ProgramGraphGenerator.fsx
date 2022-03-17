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

// module ProgramGraphGenerator

let mutable nodeIndex = 0
let mutable continuationNode = 0
let mutable currentNode = 0
//let programGraph = []


// Add new production to the parser, where we produce a parenthisized Aexrp
let rec getTextAri e =
  match e with
    | Num(x) -> string x
    | Identifier(x) -> x
    | IdentifierArray(x, y) -> x + "[" + getTextAri(y) + "]"
    | TimesExpr(x,y) -> getTextAri(x) + " * " + getTextAri (y)
    | DivExpr(x,y) -> getTextAri(x) + " / " + getTextAri (y)
    | PlusExpr(x,y) -> getTextAri(x) + " + " + getTextAri (y)
    | MinusExpr(x,y) -> getTextAri(x) + " - " + getTextAri (y)
    | PowExpr(x,y) -> getTextAri(x) + " ^ " + getTextAri (y)
    | UPlusExpr(x) -> "+" + getTextAri(x)
    | UMinusExpr(x) -> "-" + getTextAri(x)

let rec getTextBool e =
    match e with
      | Bool(x) -> string x
      | NotExpr(x) -> getTextBool(x) + ")"
      | LOrExpr(x,y) -> getTextBool(x) + " || " + getTextBool(y)
      | LAndExpr(x,y) -> getTextBool(x) + " && " + getTextBool(y)
      | OrExpr(x,y) -> getTextBool(x) + " | " + getTextBool(y)
      | AndExpr(x,y) -> getTextBool(x) + " & " + getTextBool(y)
      | EqExpr(x,y) -> getTextAri(x) + " = " + getTextAri(y)
      | NeqExpr(x,y) -> getTextAri(x) + " != " + getTextAri(y)
      | LtExpr(x,y) -> getTextAri(x) + " < " + getTextAri(y)
      | LeqExpr(x,y) -> getTextAri(x) + " <=" + getTextAri(y)
      | GtExpr(x,y) -> getTextAri(x) + " > " + getTextAri(y)
      | GeqExpr(x,y) -> getTextAri(x) + " >= " + getTextAri(y)

let rec getAexprValue e vars =
  match e with
    | Num(x) -> x
    | Identifier(x) -> let mutable r = 0
                       List.iteri(fun i (id, value) -> if id = x then r <- value) vars
                       r
    | IdentifierArray(x, y) -> 1
    | TimesExpr(x,y) -> getAexprValue x vars  * getAexprValue y vars
    | DivExpr(x,y) -> getAexprValue x vars  / getAexprValue y vars
    | PlusExpr(x,y) -> getAexprValue x vars  + getAexprValue y vars
    | MinusExpr(x,y) -> getAexprValue x vars  - getAexprValue y vars
    | PowExpr(x,y) -> 2
    | UPlusExpr(x) -> getAexprValue x vars 
    | UMinusExpr(x) -> - getAexprValue x vars 


let parseInitialization s =
    [ ("x", 3); ("y", 5); ("z", 4) ]

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


// let rec stepExecute nodes vars=
//     Console.ReadKey() |> ignore
//     match nodes with
//         | Node(num, edge) -> match edge with
//                                 Edge(prev, act, nextNode) -> currentNode <- num
//                                                              printVars vars
//                                                              let res = performCalc act vars
//                                                              stepExecute nextNode res
//         | EndNode -> currentNode <- -1
//                      printVars vars



// let addNodeToEdge edge node =
//     match edge with
//         | Edge(prev, act, next) -> Edge(act, node)

// let combineNodeSeq node1 node2 =
//     match node1 with
//         | Node(num, edges) ->  Node(num, addNodeToEdge edges node2) 


let rec generateCommandEdges (e, startNode, endNode, programGraph) =
  match e with
    | Assign(x,y) -> nodeIndex <- nodeIndex + 1
                     continuationNode <- endNode
                     programGraph @ [Edge(startNode, Assignment(x, y), endNode)]
    | Skip  ->  nodeIndex <- nodeIndex + 1
                continuationNode <- endNode
                programGraph @ [Edge(startNode, SkipAction, endNode)]

    | CommandSeq(x,y) -> let a = generateCommandEdges(x, continuationNode, nodeIndex + 1, programGraph)
                         let b = generateCommandEdges(y, continuationNode, nodeIndex + 1, a)
                         b
    // | IfFi(x) -> 

// let rec generateGCEdges e startNode endNode = 
//     match e with
//         | guardedcommand()


let printAction act =
    match act with
        | Assignment(x, y) -> getTextAri x + " := " + getTextAri y
        | SkipAction -> "Skip"
        | Boolean(x) -> getTextBool x

let rec getEdgeString edge=
    match edge with
        | Edge(orig, action, dest) -> "(" + string orig + ")" + (printAction action) + "(" + string dest + ")"

let printProgramGraph graph =
    List.iteri (fun i e -> printfn "%s" (getEdgeString e)) graph

let getProgramGraph e =
    let graph = generateCommandEdges (e, 0 , 1, [])
    printProgramGraph graph


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
    getProgramGraph e

    //let a = stepExecute nodes vars
    printfn ""

compileFromFile 0
