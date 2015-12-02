{
  open Little_bang_generated_parser;;
}

let digit = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
let whitespace = [' ' '\t' '\n']
let comment = '#' [^'\n']* '\n'
let integer = '-'? ('0' | ['1'-'9'] (digit | '_')*)

let ident_start = '_' | alpha | '$' (*$ identifiers are for internal identifiers*)
let ident_cont = '_' | alpha | digit

rule token = parse
  | eof                              { EOF }
  | comment                          { token lexbuf }
  | whitespace                       { token lexbuf }
  | "="                              { EQUALS }
  | "&"                              { AMPERSAND }
  | "->"                             { ARROW }
  | "()"                             { EMPTY_ONION }
  | "("                              { LEFT_PAREN }
  | ")"                              { RIGHT_PAREN }
  | "*"                              { ASTERISK }
  | "+"                              { PLUS }
  | "-"                              { MINUS }
  | "=="                             { EQUALITY }
  | "<"                              { LESS_THAN }
  | "fun"                            { KEYWORD_FUN }
  | "let"                            { KEYWORD_LET }
  | "in"                             { KEYWORD_IN }
  | "ref"                            { KEYWORD_REF }
  | "int"                            { KEYWORD_INT }
  | "array"                          { KEYWORD_ARRAY }
  | "arrayNew"                       { KEYWORD_ARRAY_NEW }
  | "arrayGet"                       { KEYWORD_ARRAY_GET }
  | "arraySet"                       { KEYWORD_ARRAY_SET }
  | "arrayLength"                    { KEYWORD_ARRAY_LENGTH }
  | ":"                              { COLON }
  | ident_start ident_cont* as s     { IDENTIFIER s }
  | "`" (ident_cont* as s)           { LABEL s }
  | integer as s                     { INT (int_of_string (s)) }
  | ";;"                             { DOUBLE_SEMICOLON }
