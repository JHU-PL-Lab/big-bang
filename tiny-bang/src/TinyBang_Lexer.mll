(* file: TinyBang_Lexer.mll *)
{
  open TinyBang_Parser;;
}

let digit = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
let whitespace = [' ' '\t' '\n']

let identStart = alpha
let identCont = alpha | digit

rule token = parse
  | whitespace                       { token lexbuf }
  | "{"                              { OPEN_BRACE }
  | "}"                              { CLOSE_BRACE }
  | "\\"                             { BACKSLASH }
  | ";"                              { SEMICOLON }
  | "="                              { EQUALS }
  | "&"                              { AMPERSAND }
  | "->"                             { ARROW }
  | "()"                             { EMPTY_ONION }
  | "*"                              { ASTERISK }
  | identStart identCont* as s       { IDENTIFIER s }
  | "`" (identCont* as s)            { LABEL s }
