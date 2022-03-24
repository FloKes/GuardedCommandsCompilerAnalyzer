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

#load "ProgramGraphGenerator.fs"
open ProgramGraphGenerator


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

    let lastNode = (getDepth e) + 1
    printfn "Last node will be: %d" lastNode
    // getDepth e |> ignore
    //let a = stepExecute nodes vars
    printfn ""

compileFromFile 0
