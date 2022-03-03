// Signature file for parser generated by fsyacc
module Parser
type token = 
  | ASSIGN
  | TIMES
  | DIV
  | PLUS
  | MINUS
  | POW
  | SQRT
  | LPAR
  | RPAR
  | EOF
  | VAR of (string)
  | NUM of (int)
type tokenId = 
    | TOKEN_ASSIGN
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_POW
    | TOKEN_SQRT
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_EOF
    | TOKEN_VAR
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_expression
    | NONTERM_expression0
    | NONTERM_expression1
    | NONTERM_expression2
    | NONTERM_expression3
    | NONTERM_expression4
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (expr) 
