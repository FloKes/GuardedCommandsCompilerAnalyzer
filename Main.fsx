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


type Sign = Nsign | Zsign | Psign
type SignMem = (string * Sign) list * (string * Sign Set) list
type AnaAssign = int * SignMem Set

let filterAnaAssign  (set: Set<AnaAssign>) (node:int) =
    set |> Set.filter (fun (n,_) -> n = node)

let getAnaAssing (set: Set<AnaAssign>) (node:int) =
    let mutable res = Set.empty
    set |> Set.iter (fun (n, memSet) -> if n = node then res <- memSet)
    res

let setAnaAssign (set: AnaAssign Set) (node:int) (memSet: SignMem Set) =
    set |> Set.map (fun (n, value) -> if n = node then AnaAssign(node, memSet) else AnaAssign(n, value))
    

let getInitSignMem n =
    // SignMem([("x", Zsign); ("y", Psign); ("z", Nsign)],[("A", Set.empty.Add(Zsign).Add(Psign))])
    SignMem([("x", Psign); ("y", Nsign); ("z", Psign)],[])

let getSignStr sign =
    match sign with
        | Nsign -> "-"
        | Zsign -> "0"
        | Psign -> "+"

let getSignSetStr (signSet: Sign Set) =
    let mutable s = "{"
    signSet |> Set.iter (fun x -> s <- s + (getSignStr x) + ",")
    s <- s.Remove(s.Length - 1)
    s <- s + "}"
    s

let printSignMem (mem: SignMem) =
    let mutable str = ""
    let (varMem, arrMem) = mem
    varMem |> List.iter (fun (var, sign) -> printfn "%s: %s" var (getSignStr sign))
    arrMem |> List.iter (fun (arr, signSet) -> printfn "%s: %s" arr ( getSignSetStr signSet))
    
let printSignMemSet (memSet: SignMem Set) = 
    memSet |> Set.iter (fun signMem -> printSignMem signMem)

let printAnaAssign (ana: Set<AnaAssign>) =
    ana |> Set.iter (fun (node, mem) -> printfn "q%d: \\\\\\\\\\\\\\" node
                                        printfn "--------"  
                                        printSignMemSet mem
                                        printfn "--------" )

let getSignOfInt n =
    if n = 0 then Zsign
    else if n < 0 then Nsign
    else Psign

let getSignOfIdenArr (id: string) (mem: SignMem) =
    let mutable temp = 0
    let (vars, arrs) = mem
    //vars |> List.iter(fun (id, value) -> if id = var then temp <- value                                             )
    temp

let getSignOfIdenVar (var: string) (mem: SignMem) =
    let mutable temp = Set.empty
    let (vars, arrs) = mem
    vars |> List.iter(fun (id, sign) -> if id = var then temp <- Set.empty.Add(sign))
    temp

let signOpSet (signSet1: Sign Set) (signSet2: Sign Set) op =
    let mutable temp = Set.empty
    signSet1 |> Set.iter (fun x -> 
                                  signSet2 |> Set.iter (fun y -> temp <- Set.union temp (op x y)) )
    temp

let signAdd sign1 sign2 = 
    match sign1 with 
        | Nsign -> match sign2 with
                    | Nsign -> Set.empty.Add(Nsign)
                    | Zsign -> Set.empty.Add(Nsign)
                    | Psign -> Set.empty.Add(Nsign).Add(Zsign).Add(Psign)
        | Zsign -> match sign2 with 
                    | Nsign -> Set.empty.Add(Nsign)
                    | Zsign -> Set.empty.Add(Zsign)
                    | Psign -> Set.empty.Add(Psign)
        | Psign -> match sign2 with
                    | Nsign -> Set.empty.Add(Nsign).Add(Zsign).Add(Psign)  
                    | Zsign -> Set.empty.Add(Psign)
                    | Psign -> Set.empty.Add(Psign)    
                                
let signGte sign1 sign2 = 
    match sign1 with 
        | Nsign -> match sign2 with
                    | Nsign -> Set.empty.Add(true).Add(false)
                    | Zsign -> Set.empty.Add(false)
                    | Psign -> Set.empty.Add(false)
        | Zsign -> match sign2 with 
                    | Nsign -> Set.empty.Add(true)
                    | Zsign -> Set.empty.Add(true)
                    | Psign -> Set.empty.Add(false)
        | Psign -> match sign2 with
                    | Nsign -> Set.empty.Add(true)
                    | Zsign -> Set.empty.Add(true)
                    | Psign -> Set.empty.Add(true).Add(false)



let rec ASign (ae: aexpr) (mem: SignMem) =
    match ae with 
        | Num(x) -> Set.empty.Add(getSignOfInt x)
        | Identifier(x) -> getSignOfIdenVar x mem
        | IdentifierArray(x, y) -> Set.empty
        | PlusExpr(x, y) -> signOpSet (ASign x mem) (ASign y mem) signAdd

let rec BSign (be: bexpr) (mem: SignMem) = 
    match be with   
        | Bool(x) -> Set.empty.Add(x)
        | GeqExpr(x,y) -> signOpSet (ASign x mem) (ASign y mem) signGte


// let ExprSignSet expr (mem: SignMem Set) expSignFun = 
//     let mutable temp = Set.empty
//     mem |> Set.iter (fun x -> temp <- Set.union temp (expSignFun expr x))
//     temp

let mapVarToSign (mem: SignMem) var (sign: Sign) =
    let (varMem, arrMem) = mem
    let newVarMem = varMem |> List.map (fun (id, value) -> if id = var then (id, sign) else (id, value))
    SignMem(newVarMem, arrMem)

let AnaFun (act: action) (mem: SignMem)=
    match act with
        | Boolean(x) -> let res = BSign x mem
                        if Set.contains true res then Set.empty.Add(mem)
                        else Set.empty

        | SkipAction -> Set.empty.Add(mem)
        | Assignment(x, y) -> match x with
                                | Identifier(var) -> let mutable temp = Set.empty
                                                     let res = ASign y mem
                                                     res |> Set.iter (fun sign -> temp <- Set.union temp (Set.empty.Add(mapVarToSign mem var sign))) 
                                                     temp
                                | IdentifierArray(arr, n) -> Set.empty


let AnaFunSet (act: action) (memSet: SignMem Set)=
    let mutable temp = Set.empty
    memSet |> Set.iter (fun mem -> temp <- Set.union temp (AnaFun act mem))
    temp


let assignEmptyAnalysis (pg: list<int * action * int>) =
    let mutable res = Set.empty
    pg |> List.iter (fun (s, _, e) -> if s <> 0 then res <- res.Add(s, Set.empty)
                                      if e <> 0 then res <- res.Add(e, Set.empty))
    res |> Set.map (fun (x, s) -> AnaAssign(x,s))

let printWorklist set =
    set |> Set.iter (fun x -> printfn "q%d " x)

let rec solve (pg: list<int * action * int>) (mem: SignMem) =
    let initNodeAs = AnaAssign(0, Set.empty.Add(mem))
    let mutable anaAs = Set.union (Set.empty.Add(initNodeAs)) (assignEmptyAnalysis pg)

    let mutable worklist = Set.empty.Add(0)
    while not worklist.IsEmpty do
        // printWorklist worklist
        // Console.ReadLine()
        let q = worklist.MinimumElement
        worklist <- worklist.Remove q
        // printWorklist worklist
        // Console.ReadLine()
        let edges = filterEdgesByStart (pg, q)
        // printProgramGraph edges
        // Console.ReadLine()
        edges |> List.iter (fun (s, act, e) ->  let a0 = getAnaAssing anaAs s
                                                // printSignMemSet a0
                                                // Console.ReadLine()
                                                let res = AnaFunSet act a0
                                                // printSignMemSet res
                                                // Console.ReadLine()
                                                let mutable a1 = getAnaAssing anaAs e
                                                if not (res.IsSubsetOf a1) then
                                                    a1 <- Set.union a1 res
                                                    anaAs <- setAnaAssign anaAs e a1
                                                    worklist <- worklist.Add(e))
    printAnaAssign anaAs

    
let compileFromFile n =
    let e = parse(System.IO.File.ReadAllText (__SOURCE_DIRECTORY__ + "\GCLExamples\simple.txt"))
    // printf "Do you want to see edge steps (0:no, 1:yes): "
    // edgeSteps <- int (Console.ReadLine())
    let pg = getProgramGraph e
 //   printProgramGraph pg
    DotWriter.writeProgramGraph pg

    solve pg (getInitSignMem 0)
    // let mem = initializeMemory 0
    // printfn "%s" (getMemString mem)
    // printfn ""
    // startStepWiseExecute (pg, mem)

    // let lastNode = (getDepth e) + 1
    // printfn "Last node will be: %d" lastNode
    printfn ""

compileFromFile 0
