// #load "Interpreter.fsx"
// open TypesAST

let mutable nodeIndex = 1
let mutable joinNode = 0

let path = __SOURCE_DIRECTORY__ + "\programGraph.dot"
let init f=
    System.IO.File.WriteAllText( path, "digraph ProgramGraph{")

let writeLine s=
    System.IO.File.AppendAllText( path, "\n" + s)

let finish f=
    System.IO.File.AppendAllText( path, "\n}")

let nodeText n =
    "\"q" + (string n) + "\""

let labelText s=
    "[label=\" " + s + " \"]"

let edge (node1, node2)=
    nodeIndex <- nodeIndex + 1
    (nodeText node1) + " -> " + (nodeText (node2))
    

// Add new production to the parser, where we produce a parenthisized Aexrp
let rec getTextAri e =
  match e with
    | Num(x) -> string x
    | Identifier(x) -> x
    | IdentifierArray(x, y) -> x + "[" + getTextAri(y) + "]"
    | TimesExpr(x,y) -> getTextAri(x) + " * " + getTextAri (y)
    | DivExpr(x,y) -> getTextAri(x) + " / " + getTextAri (y)
    | PlusExpr(x,y) -> getTextAri(x) + " + " + getTextAri (y)
    | MinusExpr(x,y) -> getTextAri(x) + " - " + getTextAri (y)
    | PowExpr(x,y) -> getTextAri(x) + " ^ " + getTextAri (y)
    | UPlusExpr(x) -> "+" + getTextAri(x)
    | UMinusExpr(x) -> "-" + getTextAri(x)

let rec getTextBool e =
    match e with
      | Bool(x) -> string x
      | NotExpr(x) -> getTextBool(x) + ")"
      | LOrExpr(x,y) -> getTextBool(x) + " || " + getTextBool(y)
      | LAndExpr(x,y) -> getTextBool(x) + " && " + getTextBool(y)
      | OrExpr(x,y) -> getTextBool(x) + " | " + getTextBool(y)
      | AndExpr(x,y) -> getTextBool(x) + " & " + getTextBool(y)
      | EqExpr(x,y) -> getTextAri(x) + " = " + getTextAri(y)
      | NeqExpr(x,y) -> getTextAri(x) + " != " + getTextAri(y)
      | LtExpr(x,y) -> getTextAri(x) + " < " + getTextAri(y)
      | LeqExpr(x,y) -> getTextAri(x) + " <=" + getTextAri(y)
      | GtExpr(x,y) -> getTextAri(x) + " > " + getTextAri(y)
      | GeqExpr(x,y) -> getTextAri(x) + " >= " + getTextAri(y)


let rec writeCommandEdges (e, startNode, endNode) =
  match e with
    | Assign(x,y) -> writeLine (edge(startNode, endNode) + (labelText (getTextAri(x) + " := " + getTextAri (y))))
                     int (endNode)
    | Skip  -> writeLine (edge(startNode, endNode) + (labelText "skip"))
               int (endNode)
    | CommandSeq(x,y) -> let newEnd = writeCommandEdges(x, startNode, endNode) 
                         writeCommandEdges(y, newEnd, newEnd + 1)
    | IfFi(x) -> getASTGuardedCommand(x, startNode, endNode, endNode + 1)
    // | DoOd(x) -> getASTGuardedCommand(x, node, node)

and getASTGuardedCommand (e, startNode, nodeNumber, nextNode) =
    match e with
        | GuardedCommand(x, y) -> writeLine(edge(startNode, nodeNumber) + (labelText (getTextBool(x))))
                                  writeCommandEdges(y, nodeNumber, nextNode)
                                //   let lastNode = int (writeCommandEdges(y, node + 1))
                                //   writeLine(edge(node, lastNode) + (labelText ("!(" + getTextBool(x) + ")")))
                                //   int lastNode
                                  
         | GuardedCommandSeq(x,y) -> let joinNode = getASTGuardedCommand(x, startNode, nodeNumber, nextNode)
                                     getASTGuardedCommand(y, startNode, nodeIndex, joinNode )


let printProgramGraph e =
    init 0
    writeCommandEdges(e, 0, 1)
    finish 0

