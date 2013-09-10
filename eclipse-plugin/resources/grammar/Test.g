grammar Test;

options {
  language = Java;
  backtrack = true;
  memoize = true;
  output = AST;
}

program: expression EOF;

expression
  :
    primaryExpression OPERATOR expression
  | primaryExpression
  ;

primaryExpression
  :
  INTEGER
  | IDENTIFIER
  | parenthesizedExpression
  ;

parenthesizedExpression
  :
  '(' expression ')'
  ;

WHITESPACE
    :   (
             ' '
        |    '\r'
        |    '\t'
        |    '\u000C'
        |    '\n'
        ) 
            {
                skip();
            }          
    ;

OPERATOR
  :
  '+'
  | '-'
  ;

INTEGER
  :
  '-'? ('0'..'9')+
  ;

IDENTIFIER
  :
  (
    'a'..'z'
    | 'A'..'Z'
    | '_'
  )
  (
    'a'..'z'
    | 'A'..'Z'
    | '_'
    | '0'..'9'
    | '\''
  )
  ;
