{
  open Tiny_bang_generated_parser;;
}

let digit = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
let whitespace = [' ' '\t' '\n']
let comment = '#' [^'\n']* '\n'
let integer =  '-'? ('0' | ['1'-'9'] (digit | '_')*)

let ident_start = alpha
let ident_cont = alpha | digit

rule token = parse
  | eof                              { EOF }
  | "int+"                           { INT_PLUS }
  | "int-"                           { INT_MINUS }
  | "int*"                           { INT_TIMES }
  | "int="                           { INT_EQUAL }
  | "int<"                           { INT_LESSTHAN }
  | "arrayNew"                       { ARRAY_NEW }
  | "arrayLength"                    { ARRAY_LENGTH }
  | "arrayGet"                       { ARRAY_GET }
  | "arraySet"                       { ARRAY_SET }
  | "array"                          { ARRAY }
  | comment                          { token lexbuf }
  | whitespace                       { token lexbuf }
  | "{"                              { OPEN_BRACE }
  | "}"                              { CLOSE_BRACE }
  | "\\"                             { BACKSLASH }
  | ";"                              { SEMICOLON }
  | ":"                              { COLON }
  | "="                              { EQUALS }
  | "&"                              { AMPERSAND }
  | "->"                             { ARROW }
  | "()"                             { EMPTY_ONION }
  | "*"                              { ASTERISK }
  | "int"                            { KEYWORD_INT }
  | "ref"                            { KEYWORD_REF }
  | ident_start ident_cont* as s     { IDENTIFIER s }
  | "`" (ident_cont* as s)           { LABEL s }
  | ";;"                             { DOUBLE_SEMICOLON }
  | integer as s                     { INT (int_of_string (s)) }
  | ":="                             { REFERENCE_ASSIGN }
