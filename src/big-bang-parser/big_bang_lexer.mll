{
  open Big_bang_parser;;
}

(***********************)
(* Regular expressions *)
(***********************)

(* Helpers. *)

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']

(* Whitespace. *)

let newline = '\r' | '\n' | "\r\n"

let whitespace = [' ' '\t']+

(* Comments. *)

let comment = '#' [^'\n']* '\n'

(* Identifier. *)

let identifier_start = letter | '_'
let identifier_continuation = letter | digit | ['_' '?' '!' '\'']

(* Literals. *)

let integer =  '-'? ('0' | ['1'-'9'] (digit | '_')*)

(****************)
(* Lexing rules *)
(****************)

rule read = parse

  (* Symbols. *)

  | "="  { EQUALS }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { ASTERISK }
  | "/"  { SLASH }
  | "&"  { AMPERSAND }
  | "."  { DOT }
  | ","  { COMMA }
  | ";"  { SEMICOLON }
  | "_"  { UNDERSCORE }
  | ";;" { DOUBLE_SEMICOLON }

  (* Grouping symbols. *)

  | "(" { LEFT_PARENTHESIS }
  | ")" { RIGHT_PARENTHESIS }
  | "[" { LEFT_SQUARE_BRACKET }
  | "]" { RIGHT_SQUARE_BRACKET }
  | "{" { LEFT_CURLY_BRACKET }
  | "}" { RIGHT_CURLY_BRACKET }

  (* Operators. *)

  | "=?"    { OPERATOR_EQUALITY }
  | "not=?" { OPERATOR_INEQUALITY }
  | "<?"    { OPERATOR_LESS_THAN }

  (* Keywords. *)

  | "true"                 { TRUE }
  | "false"                { FALSE }
  | "and"                  { AND }
  | "or"                   { OR }
  | "xor"                  { XOR }
  | "not"                  { NOT }
  | "fun"                  { FUN }
  | "def"                  { DEF }
  | "let"                  { LET }
  | "ref"                  { REF }
  | "if"                   { IF }
  | "then"                 { THEN }
  | "else" whitespace "if" { ELSE_IF }
  | "else"                 { ELSE }
  | "end"                  { END }
  | "match"                { MATCH }
  | "as"                   { AS }
  | "in"                   { IN }
  | "repeat"               { REPEAT }
  | "while"                { WHILE }
  | "do"                   { DO }
  | "return"               { RETURN }
  | "object"               { OBJECT }
  | "include"              { INCLUDE }
  | "public"               { PUBLIC }
  | "private"              { PRIVATE }

  (* Literals. *)

  | integer                                       { INTEGER (int_of_string (Lexing.lexeme lexbuf)) }
  | '\'' (_ as character_value) '\''              { CHARACTER character_value }
  | '"' ([^'"']*? as text_value) '"'              { TEXT (text_value) }
  | "`" (identifier_continuation+ as label_value) { LABEL label_value }

  (* Comments. *)

  | comment { read lexbuf }

  (* Whitespace. *)

  | whitespace { read lexbuf }

  (* Identifier. *)

  | identifier_start identifier_continuation* as identifier_value { IDENTIFIER identifier_value }

  (* Delimiters. *)

  | newline+ { NEWLINE }
  | eof      { EOF }
