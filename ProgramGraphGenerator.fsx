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
let programGraph = []


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

let rec stepExecute nodes vars=
    Console.ReadKey() |> ignore
    match nodes with
        | Node(num, edge) -> match edge with
                                Edge(act, nextNode) -> currentNode <- num
                                                       printVars vars
                                                       let res = performCalc act vars
                                                       stepExecute nextNode res
        | DummyNode -> currentNode <- -1
                       printVars vars




let addNodeToEdge edge node =
    match edge with
        | Edge(act, next) -> Edge(act, node)

let combineNodeSeq node1 node2 =
    match node1 with
        | Node(num, edges) ->  Node(num, addNodeToEdge edges node2) 


let rec generateCommandEdges (e, startNode, endNode) =
  match e with
    | Assign(x,y) -> nodeIndex <- nodeIndex + 1
                     continuationNode <- endNode
                     Node(startNode, Edge(Assignment(x, y), DummyNode))
    | Skip  ->  nodeIndex <- nodeIndex + 1
                continuationNode <- endNode
                Node(startNode, Edge(SkipAction, DummyNode))

    | CommandSeq(x,y) -> let a = generateCommandEdges(x, continuationNode, nodeIndex + 1)
                         let b = generateCommandEdges(y, continuationNode, nodeIndex + 1)
                         combineNodeSeq a b


let printAction act =
    match act with
        | Assignment(x, y) -> "Assignment"
        | SkipAction -> "Skip"

let rec getNodeString node=
    match node with
        | Node(x, y) -> match y with
                            | Edge(action, nextNode) -> "(" + string x + ")" + (printAction action) + getNodeString nextNode
        | DummyNode -> "(End)"

let getProgramGraph e =
    let nodes = generateCommandEdges (e, 0 , 1)
    printfn "%s" (getNodeString nodes)
    nodes


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
    let nodes = getProgramGraph e
    let s = Console.ReadLine()
    let vars = parseInitialization s

    let a = stepExecute nodes vars
    printfn ""

    
compileFromFile 0
