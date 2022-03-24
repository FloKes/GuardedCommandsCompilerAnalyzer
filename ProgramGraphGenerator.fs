module ProgramGraphGenerator

let mutable freshNodeIndex = 1
let mutable edgeSteps = 0

let incFreshNodeIndex() = freshNodeIndex <- freshNodeIndex + 1
let decFreshNodeIndex() = 
    freshNodeIndex <- freshNodeIndex - 1
    printfn "Node decremented to %d" freshNodeIndex

let rec getEdgeString edge=
    match edge with
        | (orig, action, dest) -> "(" + string orig + ")" + (printAction action) + "(" + string dest + ")"

let getEdgeTuple edge=
    match edge with
        | (orig, action, dest) -> "(" + string orig + ", " + (printAction action) + ", " + string dest + ")"

let printProgramGraph graph =
    List.iteri (fun i e -> printfn "%s" (getEdgeString e)) graph

let printEdgeTuples list = 
    printf "["
    list |> List.iter (fun e -> printf "%s" (getEdgeTuple e))
    printf "]"
    printfn ""

let updateNodeIndex pg =
    let mutable max = freshNodeIndex - 1
    pg |> List.iter(fun (s, _, e) -> if s > max then max <- s
                                     if e > max then max <- e)
    freshNodeIndex <- max + 1

let joinLists list1 list2 = 
    let a = list1 @ list2
    if edgeSteps = 1 then
        Console.ReadLine() |> ignore
        printEdgeTuples a
    updateNodeIndex a
    a

let getFresh n =
    printfn "get fresh: %d" (freshNodeIndex + n)
    freshNodeIndex + n

let rec generateCommandEdges (e, startNode, endNode, programGraph) =
  match e with
    | Assign(x,y) -> joinLists programGraph [Edge(startNode, Assignment(x, y), endNode)]
                
    | Skip  -> joinLists programGraph [Edge(startNode, SkipAction, endNode)]

    | CommandSeq(x,y) -> let depthx = getDepth x
                         let fresh = getFresh depthx
                         let a = generateCommandEdges(x, startNode, fresh, programGraph)
                        
                         generateCommandEdges(y, fresh, endNode, a)

    | IfFi(gc) ->   generateGCEdges (gc, startNode, endNode, programGraph)
                    
    | DoOd(gc) -> let doneGc = [Edge(startNode, SkipAction, endNode)]
                  let edges = generateGCEdges (gc, startNode, startNode, programGraph)
                  edges @ doneGc

and generateGCEdges (e, startNode, endNode, programGraph) = 
    match e with
        | GuardedCommand(b, exp) -> let fresh = getFresh 0
                                    let a = joinLists programGraph [Edge(startNode, Boolean(b), fresh)]
                                    let b = generateCommandEdges(exp, fresh, endNode, a)
                                    b
                                    
        | GuardedCommandSeq(gc1, gc2) -> let edgesGC1 = generateGCEdges(gc1, startNode, endNode, programGraph)                                        
                                         generateGCEdges(gc2, startNode, endNode, edgesGC1)
                                         

let getProgramGraph e =
    let lastNode = (getDepth e) + 1
    printfn "Last node will be: %d" lastNode
    let graph = generateCommandEdges (e, 0 , lastNode, [])
    graph 


// input parser
let parse input =
    let lexbuf = LexBuffer<char>.FromString input
    let res = Parser.start Lexer.tokenize lexbuf
    res