// This script implements our interactive calculator

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

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)

let rec getASTAri e =
  match e with
    | Num(x) -> string x
    | Identifier(x) -> x
    | IdentifierArray(x, y) -> x + "[" + getASTAri(y) + "]"
    | TimesExpr(x,y) -> "*(" + getASTAri(x) + ", " + getASTAri (y) + ")"
    | DivExpr(x,y) -> "/(" + getASTAri(x) + ", " + getASTAri (y) + ")"
    | PlusExpr(x,y) -> "+(" + getASTAri(x) + ", " + getASTAri (y) + ")"
    | MinusExpr(x,y) -> "-(" + getASTAri(x) + ", " + getASTAri (y) + ")"
    | PowExpr(x,y) -> "pow(" + getASTAri(x) + ", " + getASTAri (y) + ")"
    | UPlusExpr(x) -> "+" + getASTAri(x)
    | UMinusExpr(x) -> "-" + getASTAri(x)

let rec getASTBool e =
    match e with
      | Bool(x) -> string x
      | NotExpr(x) -> "!(" + getASTBool(x) + ")"
      | LOrExpr(x,y) -> "||(" + getASTBool(x) + ", " + getASTBool(y) + ")"
      | LAndExpr(x,y) -> "&&(" + getASTBool(x) + ", " + getASTBool(y) + ")"
      | OrExpr(x,y) -> "|(" + getASTBool(x) + ", " + getASTBool(y) + ")"
      | AndExpr(x,y) -> "&(" + getASTBool(x) + ", " + getASTBool(y) + ")"
      | EqExpr(x,y) -> "=(" + getASTAri(x) + ", " + getASTAri(y) + ")"
      | NeqExpr(x,y) -> "!=(" + getASTAri(x) + ", " + getASTAri(y) + ")"
      | LtExpr(x,y) -> "<(" + getASTAri(x) + ", " + getASTAri(y) + ")"
      | LeqExpr(x,y) -> "<=(" + getASTAri(x) + ", " + getASTAri(y) + ")"
      | GtExpr(x,y) -> ">(" + getASTAri(x) + ", " + getASTAri(y) + ")"
      | GeqExpr(x,y) -> ">=(" + getASTAri(x) + ", " + getASTAri(y) + ")"


let rec getASTCommand e =
  match e with
    | Assign(x,y) -> ":=(" + getASTAri(x) + "," + getASTAri (y) + ")"
    | IfFi(x) -> "IF(" + getASTGuardedCommand(x) + ") FI"
    | DoOd(x) -> "DO(" + getASTGuardedCommand(x) + ") OD"
    | CommandSeq(x,y) -> ";(" + getASTCommand(x) + ", " + getASTCommand(y) + ")"
    | Skip  -> "skip"

and getASTGuardedCommand e =
    match e with
        | GuardedCommand(x, y) -> "(" + getASTBool(x) + ") THEN(" + getASTCommand(y) + ")"
        | GuardedCommandSeq(x,y) -> getASTGuardedCommand(x) + " [] " + getASTGuardedCommand(y)
    

let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = Parser.start Lexer.tokenize lexbuf

    // return the result of parsing (i.e. value of type "expr")
    res


// We implement here the function that interacts with the user
let compileFromFile n =
    // We parse the input string
    try
    let e = parse(System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\gclexample.txt"))
    // and print the result of evaluating it
    printfn "AST: %s" (getASTCommand(e))
    with err -> printfn("Sometin wron")
    

// We implement here the function that interacts with the user
let rec compileInteractive n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter an expression: "
        try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "AST: %s" (getASTCommand(e))
        compileInteractive n
        with err -> compileInteractive (n-1)


let rec chooseMethod n=
    printf "From file: 0, interactive: 1:   "
    let s = Console.ReadLine()
    
    if s = "0" then compileFromFile 0
    else compileInteractive 3

chooseMethod 0