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


and getASTCommand e =
  match e with
    | Assign(x,y) -> ":=(" + getASTAri(x) + "," + getASTAri (y) + ")"
    | IfFi(x,y) -> "IF(" + getASTBool(x) + ") THEN " + getASTCommand(y) + " FI"


let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = Parser.start Lexer.tokenize lexbuf

    // return the result of parsing (i.e. value of type "expr")
    res


// We implement here the function that interacts with the user
let rec compile n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter an expression: "
        try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "AST: %s" (getASTCommand(e))
        compile n
        with err -> compile (n-1)

// Start interacting with the user
compile 3