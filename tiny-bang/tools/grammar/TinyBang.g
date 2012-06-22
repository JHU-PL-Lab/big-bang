grammar TinyBang;

program	:	expr EOF;

expr	:	(pat '->') => pat '->' expr | assignExpr;

assignExpr:	opOnionExpr | var '=' expr 'in' expr;

opOnionExpr
	:	opEqExpr
		( '&' opEqExpr	// onion
		| '&-' proj	// subtraction
		| '&.' proj	// projection
		)*
	;

opEqExpr:	opCompExpr ('==' opCompExpr)*;

opCompExpr
	:	opAddExpr (('<=' | '>=') opAddExpr)*;

opAddExpr
	:	opApplExpr (('+' | '-') opApplExpr)*;

opApplExpr
	:	primaryExpr (primaryExpr)*;

primaryExpr
	:	'(' expr ')' | lbl qual primaryExpr | var | literal
	;

var 	:   ID;

lbl 	:   LBL;

qual    :   ('final'|'immut')?;

proj    :   tprim | lbl | 'fun';

tprim   :   'unit' | 'int' | 'char';

literal :   '(' ')' | INT | CHAR | '(' '&' ')';

pat 	:   var ':' patpri | patpri | var ;

patpri  :   tprim | lbl pat | 'any' | '(' patpri ('&' patpri)* ')' | 'fun';

ID  	:   ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*
    ;

LBL 	:   '`' ('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'`')+
    ;

INT 	:   '0'..'9'+
    ;
    
CHAR    :   '\'' 'A'..'Z' '\''; // Clearly incorrect

WS  	:   ( ' '
	        | '\t'
        	| '\r'
	        | '\n'
            ) {$channel=HIDDEN;}
	    ;


