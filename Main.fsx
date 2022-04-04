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
#load "ActionToText.fs"
open ActionToText
#load "DotWriter.fs"
open DotWriter
#load "DepthFinder.fs"
open DepthFinder
#load "ProgramGraphGenerator.fs"
open ProgramGraphGenerator
#load "StepExecutor.fs"
open StepExecutor


let rec get_predicate_assingnments nodes =
    match nodes with 
    | [] -> []
    | node :: tail -> printfn "Enter predicate for the node: %d" node
                      let choice = Console.ReadLine()
                      [(node, choice)] @ get_predicate_assingnments tail


let rec egdes_ending_with pg q =
    match pg with
    |[] -> []
    |(s, act, e) :: tail  when e = q -> [Edge(s, act, e)] @ egdes_ending_with tail q
    |head :: tail ->  egdes_ending_with tail q


let rec nodesinP pa =
    match pa with
    |[] -> []
    |(node, choice)::tail -> [node] @ nodesinP tail


let compileFromFile n =
    let e = parse(System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\GCLExamples\simple.txt"))
    // printf "Do you want to see edge steps (0:no, 1:yes): "
    // edgeSteps <- int (Console.ReadLine())
    let pg = getProgramGraph e
    printProgramGraph pg
    DotWriter.writeProgramGraph pg

    let mem = initializeMemory 0
    printfn "%s" (getMemString mem)
    printfn ""
    startStepWiseExecute (pg, mem)

    // let lastNode = (getDepth e) + 1
    // printfn "Last node will be: %d" lastNode
    printfn ""

compileFromFile 0
