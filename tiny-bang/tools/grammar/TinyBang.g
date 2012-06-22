grammar TinyBang;

program	:	expr EOF;

expr	:	var | lbl qual expr | expr '&' expr | expr '&-' proj | expr '&.' proj | expr expr | expr op expr | var '=' expr 'in' expr | literal;

var	:	ID;

lbl	:	'`' ID;

qual	:	('final'|'immut')?;

proj	:	tprim | lbl | 'fun';

tprim	:	'unit' | 'int' | 'char';

op	:	'+' | '-' | '==' | '<=' | '>=';

literal	:	'()' | INT | CHAR | '(&)' | pat '->' expr;

pat	:	var ':' patpri;

patpri	:	tprim | lbl pat | 'any' | '(' patpri ('&' patpri)* ')' | 'fun';

ID  :	('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*
    ;

INT :	'0'..'9'+
    ;
    
CHAR	:	'\'' 'A'..'Z' '\''; // Clearly incorrect

WS  :   ( ' '
        | '\t'
        | '\r'
        | '\n'
        ) {$channel=HIDDEN;}
    ;

