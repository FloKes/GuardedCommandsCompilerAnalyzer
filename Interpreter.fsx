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
let rec eval e =
  match e with
    | Num(x) -> x
    | TimesExpr(x,y) -> eval(x) * eval (y)
    | DivExpr(x,y) -> eval(x) / eval (y)
    | PlusExpr(x,y) -> eval(x) + eval (y)
    | MinusExpr(x,y) -> eval(x) - eval (y)
    | PowExpr(x,y) -> pown (eval(x)) (eval(y))
    | UPlusExpr(x) -> eval(x)
    | UMinusExpr(x) -> - eval(x)

let rec getAST e =
  match e with
    | Num(x) -> string x
    | Identifier(x) -> x
    | IdentifierArray(x, y) -> x + "[" + getAST(y) + "]"
    | Assign(x,y) -> ":=(" + getAST(x) + "," + getAST (y) + ")"
    | TimesExpr(x,y) -> "*(" + getAST(x) + "," + getAST (y) + ")"
    | DivExpr(x,y) -> "/(" + getAST(x) + "," + getAST (y) + ")"
    | PlusExpr(x,y) -> "+(" + getAST(x) + "," + getAST (y) + ")"
    | MinusExpr(x,y) -> "-(" + getAST(x) + "," + getAST (y) + ")"
    | PowExpr(x,y) -> "pow(" + getAST(x) + "," + getAST (y) + ")"
    | UPlusExpr(x) -> "+" + getAST(x)
    | UMinusExpr(x) -> "-" + getAST(x)

// We
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
        printfn "AST: %s" (getAST(e))
        compile n
        with err -> compile (n-1)

// Start interacting with the user
compile 3

// We
let lastToken input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = Parser.token_to_string(Lexer.tokenize lexbuf)

    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec printToken n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter an expression: "
        try
        // We parse the input string
        let e = lastToken (Console.ReadLine())
        // and print the result of evaluating it
        printfn "Last token: %s" (e)
        printToken n
        with err -> printToken (n-1)

//printToken 3