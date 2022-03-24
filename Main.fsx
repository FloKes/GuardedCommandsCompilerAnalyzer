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

let getVariableValue var mem =
    let mutable temp = 0
    let (vars, arrs) = mem
    vars |> List.iter(fun (id, value) -> if id = var then temp <- value
                                                     )
    temp

let rec getAexprValue (e, mem:Mem) =
  match e with
    | Num(x) -> x
    | Identifier(x) -> getVariableValue x mem
    | IdentifierArray(x, y) -> 1
    | TimesExpr(x,y) -> getAexprValue (x, mem)  * getAexprValue (y, mem)
    | DivExpr(x,y) -> getAexprValue (x, mem)  / getAexprValue (y, mem)
    | PlusExpr(x,y) -> getAexprValue (x, mem)  + getAexprValue (y, mem)
    | MinusExpr(x,y) -> getAexprValue (x, mem)  - getAexprValue (y, mem)
    | PowExpr(x,y) -> 2
    | UPlusExpr(x) -> getAexprValue (x, mem) 
    | UMinusExpr(x) -> - getAexprValue (x, mem) 

let performCalc action mem =
    match action with
        | Assignment(var, expr) ->  let varText = getTextAri var
                                    let result = getAexprValue (expr, mem) 
                                    let (vars, ari) = mem
                                    let newVars = vars |> List.mapi (fun i (id, value) -> if id = varText then (id, result) else (id, value))
                                    Mem(newVars,ari)
                                    
        | SkipAction -> mem


let filterEdgesByStart (pg:list<Edge>, node:int) = 
    pg |> List.filter (fun (s,_,_) -> s = node)


let applyActionToMemory s act e mem =
    printfn "\nEdge: (q%d, %s, q%d)" s (printAction act) e
    printfn "Before memory: %s" (getMemString mem)
    let newMem = performCalc act mem
    Console.ReadKey()
    printf "After: memory: %s" (getMemString newMem)
    Console.ReadLine()
    newMem

let rec visitEdges (edges:list<Edge>, pg:list<Edge>, mem:Mem) =
    match edges with
    | (s, act, e) :: tail ->let memNew = applyActionToMemory s act e mem 
                            let nextEdges = filterEdgesByStart (pg, e)
                            visitEdges (nextEdges, pg, memNew)
                            //visitEdges tail mem pg 
    | [] -> mem

let startStepWiseExecute (pg:list<Edge>, mem:Mem)=
    let startEdges = filterEdgesByStart (pg, 0)
    let m = visitEdges (startEdges, pg, mem)
    m


let compileFromFile n =
    let e = parse(System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\GCLExamples\simple.txt"))
    printf "Do you want to see edge steps (0:no, 1:yes): "
    edgeSteps <- int (Console.ReadLine())
    let pg = getProgramGraph e
    printProgramGraph pg
    DotWriter.writeProgramGraph pg

    let mem = initializeMemory 0
    printfn "%s" (getMemString mem)

    startStepWiseExecute (pg, mem)

    // let lastNode = (getDepth e) + 1
    // printfn "Last node will be: %d" lastNode
    // getDepth e |> ignore
    //let a = stepExecute nodes vars
    printfn ""

compileFromFile 0
