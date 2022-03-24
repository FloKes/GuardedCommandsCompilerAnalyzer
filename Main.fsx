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
    Mem([("x", 5);("y", 1);("z", 0)], [])

let checkIfVariableExists id mem =
    let mutable exists = false
    let (vars, arrs) = mem
    vars |> List.iter(fun (var,_) -> if var = id then exists <- true)
    exists

let filterEdgesByStart (pg:list<Edge>, node:int) = 
    pg |> List.filter (fun (s,_,_) -> s = node)

let getMemString (mem:Mem) = // Only works for variables so far
    let (vars, arrs) = mem
    let mutable s = ""
    vars |> List.iter(fun (id, value) -> s <- s + id + "=" + string value + ", ")
    s

let getVariableValue var mem =
    let mutable temp = 0
    let (vars, arrs) = mem
    vars |> List.iter(fun (id, value) -> if id = var then temp <- value                                             )
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

let rec evaluateBoolean bexp mem= 
    match bexp with
      | Bool(x) -> x
      | NotExpr(x) -> not (evaluateBoolean x mem)
      | LOrExpr(x,y) -> (evaluateBoolean x mem) || (evaluateBoolean y mem)
      | LAndExpr(x,y) -> (evaluateBoolean x mem) && (evaluateBoolean y mem)
    //   | OrExpr(x,y) -> (evaluateBoolean x mem) | (evaluateBoolean y mem)
    //   | AndExpr(x,y) -> (evaluateBoolean x mem) & (evaluateBoolean y mem)
      | EqExpr(x,y) -> getAexprValue(x, mem) = getAexprValue(y, mem) 
      | NeqExpr(x,y) -> getAexprValue(x, mem) <> getAexprValue(y, mem) 
      | LtExpr(x,y) -> getAexprValue(x, mem) < getAexprValue(y, mem) 
      | LeqExpr(x,y) -> getAexprValue(x, mem) <= getAexprValue(y, mem) 
      | GtExpr(x,y) -> getAexprValue(x, mem) > getAexprValue(y, mem) 
      | GeqExpr(x,y) -> getAexprValue(x, mem) >= getAexprValue(y, mem) 

// Change cos if we have performCalc its always on Assignment
let performCalc action mem =
    match action with
        | Assignment(var, expr) ->  let varText = getTextAri var
                                    if not (checkIfVariableExists varText mem) then
                                        failwith ("Update of unkown variable " + varText + " at " + varText + ":=" + getTextAri expr)

                                    //TODO : add a function which checks all the variables used in an expression
                                    // and return something ala "Lookup of unknown variable z at x:=z."
                                    let result = getAexprValue (expr, mem) 
                                    let (vars, ari) = mem
                                    let newVars = vars |> List.mapi (fun i (id, value) -> if id = varText then (id, result) else (id, value))
                                    Mem(newVars,ari)


let applyActionToMemory s act e mem =
    // printfn "\nEdge: (q%d, %s, q%d)" s (printAction act) e
    // printfn "Before memory: %s" (getMemString mem)
    let newMem = performCalc act mem
    // Console.ReadKey() |> ignore
    // printf "After: memory: %s" (getMemString newMem)
    // Console.ReadLine() |> ignore
    newMem

let printEdgeDecomposed s act e=
    printfn "\nEdge: (q%d, %s, q%d)" s (printAction act) e

let printMemAtTime time mem =
    printf "%s action: %s" time (getMemString mem)
    Console.ReadLine() |> ignore

let evaluateListOfBooleans edgeList mem =
    let mutable counter = 0
    edgeList |> List.iter (fun (_, act, _) -> match act with | Boolean(bexp) -> if (evaluateBoolean bexp mem) then counter <- counter + 1)
    counter

(*
Pattern match on type of act (act should maybe be changed to sth else)
If assign or skip, run apply action to memory, and call visit to edges on the next node
If Boolean, check if head is true. Then if there are is a tail, check if any of those are true.
If there are, continue with first branch and indicate that a non-det choice has been made
If there are no true ones say stuck!
*)
let rec visitEdges (edges:list<Edge>, pg:list<Edge>, mem:Mem) =
    match edges with
    | (s, act, e) :: tail -> match act with
                                | Assignment(_,_) ->printEdgeDecomposed s act e 
                                                    printMemAtTime "Before" mem
                                                    let memNew = applyActionToMemory s act e mem 
                                                    printMemAtTime "After" memNew
                                                    let nextEdges = filterEdgesByStart (pg, e)
                                                    visitEdges (nextEdges, pg, memNew)
                                                    //visitEdges tail mem pg 
                                | SkipAction -> printEdgeDecomposed s act e 
                                                printMemAtTime "Before" mem
                                                printMemAtTime "After" mem
                                                let nextEdges = filterEdgesByStart (pg, e)
                                                visitEdges (nextEdges, pg, mem)
                                | Boolean(bexp) ->  let headEval = evaluateBoolean bexp mem
                                                    let tailEval = evaluateListOfBooleans tail mem
                                                    if not headEval && tailEval = 0 then
                                                        failwith "Program stuck"
                                                        printfn "Program stuck"
                                                    else if headEval && tailEval > 0 then
                                                        // should make this choice random
                                                        printfn "Non deterministic choice made"
                                                        printEdgeDecomposed s act e
                                                        printMemAtTime "Before" mem
                                                        printMemAtTime "After" mem 
                                                        let nextEdges = filterEdgesByStart (pg, e)
                                                        visitEdges (nextEdges, pg, mem)
                                                    else if headEval && tailEval = 0 then
                                                        printEdgeDecomposed s act e
                                                        printMemAtTime "Before" mem
                                                        printMemAtTime "After" mem 
                                                        let nextEdges = filterEdgesByStart (pg, e)
                                                        visitEdges (nextEdges, pg, mem)
                                                    else
                                                        visitEdges (tail, pg, mem)
    | [] -> printfn "\nProgram terminated :)" 
            



// let memNew = applyActionToMemory s act e mem 
// let nextEdges = filterEdgesByStart (pg, e)
// visitEdges (nextEdges, pg, memNew)
// //visitEdges tail mem pg 

let startStepWiseExecute (pg:list<Edge>, mem:Mem)=
    let startEdges = filterEdgesByStart (pg, 0)
    let m = visitEdges (startEdges, pg, mem)
    m


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
