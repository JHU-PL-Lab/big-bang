{
  open Tiny_bang_parser_support;;
  open Tiny_bang_generated_parser;;
}

let digit = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
let whitespace = [' ' '\t' '\n']

let ident_start = alpha
let ident_cont = alpha | digit

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
  | ident_start ident_cont* as s     { IDENTIFIER s }
  | "`" (ident_cont* as s)           { LABEL s }
  | ";;"                             { DOUBLE_SEMICOLON }
