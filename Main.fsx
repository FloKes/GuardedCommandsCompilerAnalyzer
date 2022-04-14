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
#load "SignAnalyser.fs"
open SignAnalyser


type Flow = (string * string) Set

type SecLevel = string

type SecLattice = SecLevel*SecLevel

type SecClass = (string * SecLevel) list

let printFlow flow =
    flow |> Set.iter (fun (x,y) -> printf "%s → %s, " x y)
    printfn""

let makeFlow (identSet1: string Set) (identSet2: string Set) =
    let mutable tempSet = Set.empty
    identSet1 |> Set.iter (fun x ->  identSet2 |> Set.iter (fun y -> tempSet <- tempSet.Add(x,y)))
    Flow(tempSet)

let rec getAexprIdentSet e (identSet: string Set) =
  match e with
    | Num(x) -> identSet
    | Identifier(x) -> identSet.Add x
    | IdentifierArray(x, y) -> identSet.Add x
    | TimesExpr(x,y) -> Set.union (getAexprIdentSet x identSet) (getAexprIdentSet y identSet)
    | DivExpr(x,y) -> Set.union (getAexprIdentSet x identSet) (getAexprIdentSet y identSet)
    | PlusExpr(x,y) -> Set.union (getAexprIdentSet x identSet) (getAexprIdentSet y identSet)
    | MinusExpr(x,y) -> Set.union (getAexprIdentSet x identSet) (getAexprIdentSet y identSet)
    | PowExpr(x,y) -> Set.union (getAexprIdentSet x identSet) (getAexprIdentSet y identSet)
    | UPlusExpr(x) -> getAexprIdentSet x identSet
    | UMinusExpr(x) -> getAexprIdentSet x identSet


let rec getBooleanIdentSet bexp (identSet: string Set)= 
    match bexp with
      | Bool(x) -> identSet
      | NotExpr(x) -> getBooleanIdentSet x identSet
      | LOrExpr(x,y) -> Set.union (getBooleanIdentSet x identSet) (getBooleanIdentSet x identSet)
      | LAndExpr(x,y) -> Set.union (getBooleanIdentSet x identSet) (getBooleanIdentSet x identSet)
      | OrExpr(x,y) -> Set.union (getBooleanIdentSet x identSet) (getBooleanIdentSet x identSet)
      | AndExpr(x,y) -> Set.union (getBooleanIdentSet x identSet) (getBooleanIdentSet x identSet)
      | EqExpr(x,y) -> Set.union (getAexprIdentSet x identSet) (getAexprIdentSet x identSet)
      | NeqExpr(x,y) -> Set.union (getAexprIdentSet x identSet) (getAexprIdentSet x identSet)
      | LtExpr(x,y) -> Set.union (getAexprIdentSet x identSet) (getAexprIdentSet x identSet)
      | LeqExpr(x,y) -> Set.union (getAexprIdentSet x identSet) (getAexprIdentSet x identSet)
      | GtExpr(x,y) -> Set.union (getAexprIdentSet x identSet) (getAexprIdentSet x identSet)
      | GeqExpr(x,y) -> Set.union (getAexprIdentSet x identSet) (getAexprIdentSet x identSet)


let rec sec e X  =
  match e with
    | Assign(x,a) -> let fva = getAexprIdentSet a Set.empty 
                     let xSet = getAexprIdentSet x Set.empty
                     makeFlow (Set.union X fva) xSet
                
    | Skip  -> Set.empty

    | CommandSeq(x,y) -> let depthx = getDepth x
                         let fresh = getFresh depthx
                         let a = sec x  X
                         let b = sec y X
                         Set.union a b

    | IfFi(gc) ->   let (flow,_) = secG gc (Bool(false)) X
                    flow
                    
    | DoOd(gc) -> let (flow,_) = secG gc (Bool(false)) X
                  flow


and secG e d X = 
    match e with
        | GuardedCommand(b, C) -> let a = sec C (Set.union (Set.union X (getBooleanIdentSet d Set.empty)) (getBooleanIdentSet b Set.empty))
                                  let d = NotExpr(b)
                                  a, d
                                    
        | GuardedCommandSeq(gc1, gc2) -> let (w1, d1) = secG gc1 d X                                       
                                         let (w2, d2) = secG gc2 d1 X
                                         Set.union w1 w2, d2

let flowsTo (sec1: SecLevel) (sec2: SecLevel) (secLattice: SecLattice)=
    let (low,high) = secLattice
    if sec1 = high && sec2 = low
    then false
    else true

    

let computeAllowedFlows (secClass: SecClass) (secLattice: SecLattice) =
    let mutable tempSet = Set.empty
    secClass |> List.iter (fun (x, sec1) -> 
                            secClass |> List.iter (fun (y, sec2) -> if (flowsTo sec1 sec2 secLattice) 
                                                                    then tempSet <- tempSet.Add(x,y)))
    Flow(tempSet)


let findViolations (actual: Flow) (allowed: Flow) =
    let b = Set.isSubset actual allowed
    if b then Flow(Set.empty)
    else 
    let diff = Set.difference actual allowed
    Flow(diff)

let isSecure (violations: Flow) = 
    if violations.IsEmpty then printf "Secure\n" 
    else printf "Not secure\n"

let analyzeSecurity e= 
    let pub = "public"
    let priv  = "private"
    let secLattice = SecLattice(pub, priv)
    let secClass = [("x", pub); ("y", priv); ("z", pub)]
    let actualFlow = sec e Set.empty
    printf "Actual   "
    printFlow actualFlow
    printfn""

    let allowedFlow = computeAllowedFlows secClass secLattice
    printf "Allowed   "
    printFlow allowedFlow
    printfn""

    let violations = findViolations actualFlow allowedFlow
    printf "Violations   "
    printFlow violations
    printfn""

    isSecure violations

let compileFromFile n =
    let e = parse(System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\GCLExamples\simple.txt"))
    // printf "Do you want to see edge steps (0:no, 1:yes): "
    // edgeSteps <- int (Console.ReadLine())

    // Program graph generation
    // let pg = getProgramGraph e
    // printProgramGraph pg
    // DotWriter.writeProgramGraph pg

    // Stepwise executions
    // let mem = initializeMemory 0
    // printfn "%s" (getMemString mem)
    // printfn ""
    // startStepWiseExecute (pg, mem)

    // Detection of signs
    // solve pg (getInitSignMem 0)

    analyzeSecurity e

    // let lastNode = (getDepth e) + 1
    // printfn "Last node will be: %d" lastNode
    // printfn ""

compileFromFile 0
