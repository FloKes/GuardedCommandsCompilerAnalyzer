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


type Mem = (string * int) list * (string * int list) list

let initializeMemory mem = 
    Mem([("x", 1);("y", 4)], [])

let getMemString (mem:Mem) = // Only works for variables so far
    let (vars, arrs) = mem
    let mutable s = ""
    vars |> List.iter(fun (id, value) -> s <- s + id + "=" + string value + ", ")
    s

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

let performCalc action mem =
    match action with
        | Assignment(var, expr) ->  let varText = getTextAri var
                                    let result = getAexprValue expr mem 
                                    mem |> List.mapi (fun i (id, value) -> if varText = id then (varText, result) else (id, value))
                                    
        | SkipAction -> mem





let stepWiseExecute (pg:list<Edge>, mem:Mem)=
    let startEdges = pg |> List.filter (fun (s,_,_) -> s = 0)
    printProgramGraph startEdges


let compileFromFile n =
    let e = parse(System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\GCLExamples\simple.txt"))
    printf "Do you want to see edge steps (0:no, 1:yes): "
    edgeSteps <- int (Console.ReadLine())
    let pg = getProgramGraph e
    printProgramGraph pg
    DotWriter.writeProgramGraph pg

    let mem = initializeMemory 0
    printfn "%s" (getMemString mem)

    stepWiseExecute (pg, mem)

    // let lastNode = (getDepth e) + 1
    // printfn "Last node will be: %d" lastNode
    // getDepth e |> ignore
    //let a = stepExecute nodes vars
    printfn ""

compileFromFile 0
