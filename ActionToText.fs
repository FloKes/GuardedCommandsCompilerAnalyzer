module ActionToText


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


let printAction act =
    match act with
        | Assignment(x, y) -> getTextAri x + " := " + getTextAri y
        | SkipAction -> "Skip"
        | Boolean(x) -> getTextBool x


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