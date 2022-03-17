// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module TypesAST


type aexpr =
  | Num of int
  | Identifier of string
  | IdentifierArray of (string * aexpr)
  | TimesExpr of (aexpr * aexpr)
  | DivExpr of (aexpr * aexpr)
  | PlusExpr of (aexpr * aexpr)
  | MinusExpr of (aexpr * aexpr)
  | PowExpr of (aexpr * aexpr)
  | UPlusExpr of (aexpr)
  | UMinusExpr of (aexpr)

type bexpr = 
  | Bool of bool
  | NotExpr of bexpr
  | LOrExpr of (bexpr * bexpr)
  | LAndExpr of (bexpr * bexpr)
  | OrExpr of (bexpr * bexpr)
  | AndExpr of (bexpr * bexpr)
  | EqExpr of (aexpr * aexpr)
  | NeqExpr of (aexpr * aexpr)
  | LtExpr of (aexpr * aexpr)
  | LeqExpr of (aexpr * aexpr)
  | GtExpr of (aexpr * aexpr)
  | GeqExpr of (aexpr * aexpr)

type command = 
  | Assign of (aexpr * aexpr)
  | IfFi of guardedcommand
  | DoOd of guardedcommand
  | CommandSeq of (command * command)
  | Skip 
  
and guardedcommand = 
  | GuardedCommand of (bexpr * command)
  | GuardedCommandSeq of (guardedcommand * guardedcommand)



type action=
    | SkipAction
    | Assignment of (aexpr * aexpr)
    | Boolean of bexpr



type edgeTypes=
    | Edge of (int * action * int)

and nodeTypes=
  | Node of (int * edgeTypes)
  | StartNode
  | EndNode


// type edgeTypes=
//     | Edge of (action * nodeTypes)
//     | MultipleEdges of (edgeTypes * edgeTypes)

// and nodeTypes=
//   | Node of (int * edgeTypes)
//   | DummyNode