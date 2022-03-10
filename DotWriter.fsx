#load "Interpreter.fsx"
open TypesAST

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
    "[label=\"   " + s + "   \"]"

let edge (node1, node2)=
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


let rec writeCommandEdges (e, node) =
  match e with
    | Assign(x,y) -> writeLine (edge(node, node + 1) + (labelText (getTextAri(x) + " := " + getTextAri (y))))
    | Skip  -> writeLine (edge(node, node + 1) + (labelText "skip"))
    | CommandSeq(x,y) -> writeCommandEdges(x, node) 
                         writeCommandEdges(y, node + 1)
    // | IfFi(x) -> "IF(" + getASTGuardedCommand(x) + ") FI"
    // | DoOd(x) -> "DO(" + getASTGuardedCommand(x) + ") OD"

// and getASTGuardedCommand e =
//     match e with
//         | GuardedCommand(x, y) -> "(" + getTextBool(x) + ") THEN(" + getASTCommand(y) + ")"
//         | GuardedCommandSeq(x,y) -> getASTGuardedCommand(x) + " [] " + getASTGuardedCommand(y)


let getProgramGraph e =
    init 0
    writeCommandEdges(e, 0)
    finish 0

