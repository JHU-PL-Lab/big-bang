// $ANTLR 3.2 Sep 23, 2009 12:02:23 /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g 2013-09-10 14:37:43

import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

public class TestLexer extends Lexer {
    public static final int INTEGER=5;
    public static final int IDENTIFIER=6;
    public static final int OPERATOR=4;
    public static final int WHITESPACE=7;
    public static final int EOF=-1;
    public static final int T__9=9;
    public static final int T__8=8;

    // delegates
    // delegators

    public TestLexer() {;} 
    public TestLexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }
    public TestLexer(CharStream input, RecognizerSharedState state) {
        super(input,state);

    }
    public String getGrammarFileName() { return "/Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g"; }

    // $ANTLR start "T__8"
    public final void mT__8() throws RecognitionException {
        try {
            int _type = T__8;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:7:6: ( '(' )
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:7:8: '('
            {
            match('('); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__8"

    // $ANTLR start "T__9"
    public final void mT__9() throws RecognitionException {
        try {
            int _type = T__9;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:8:6: ( ')' )
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:8:8: ')'
            {
            match(')'); 

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__9"

    // $ANTLR start "WHITESPACE"
    public final void mWHITESPACE() throws RecognitionException {
        try {
            int _type = WHITESPACE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:31:5: ( ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' ) )
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:31:9: ( ' ' | '\\r' | '\\t' | '\\u000C' | '\\n' )
            {
            if ( (input.LA(1)>='\t' && input.LA(1)<='\n')||(input.LA(1)>='\f' && input.LA(1)<='\r')||input.LA(1)==' ' ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


                            skip();
                        

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WHITESPACE"

    // $ANTLR start "OPERATOR"
    public final void mOPERATOR() throws RecognitionException {
        try {
            int _type = OPERATOR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:44:3: ( '+' | '-' )
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:
            {
            if ( input.LA(1)=='+'||input.LA(1)=='-' ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "OPERATOR"

    // $ANTLR start "INTEGER"
    public final void mINTEGER() throws RecognitionException {
        try {
            int _type = INTEGER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:50:3: ( ( '-' )? ( '0' .. '9' )+ )
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:51:3: ( '-' )? ( '0' .. '9' )+
            {
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:51:3: ( '-' )?
            int alt1=2;
            int LA1_0 = input.LA(1);

            if ( (LA1_0=='-') ) {
                alt1=1;
            }
            switch (alt1) {
                case 1 :
                    // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:51:3: '-'
                    {
                    match('-'); 

                    }
                    break;

            }

            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:51:8: ( '0' .. '9' )+
            int cnt2=0;
            loop2:
            do {
                int alt2=2;
                int LA2_0 = input.LA(1);

                if ( ((LA2_0>='0' && LA2_0<='9')) ) {
                    alt2=1;
                }


                switch (alt2) {
            	case 1 :
            	    // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:51:9: '0' .. '9'
            	    {
            	    matchRange('0','9'); 

            	    }
            	    break;

            	default :
            	    if ( cnt2 >= 1 ) break loop2;
                        EarlyExitException eee =
                            new EarlyExitException(2, input);
                        throw eee;
                }
                cnt2++;
            } while (true);


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "INTEGER"

    // $ANTLR start "IDENTIFIER"
    public final void mIDENTIFIER() throws RecognitionException {
        try {
            int _type = IDENTIFIER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:55:3: ( ( 'a' .. 'z' | 'A' .. 'Z' | '_' ) ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | '\\'' ) )
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:56:3: ( 'a' .. 'z' | 'A' .. 'Z' | '_' ) ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' | '\\'' )
            {
            if ( (input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='_'||(input.LA(1)>='a' && input.LA(1)<='z') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            if ( input.LA(1)=='\''||(input.LA(1)>='0' && input.LA(1)<='9')||(input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='_'||(input.LA(1)>='a' && input.LA(1)<='z') ) {
                input.consume();

            }
            else {
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "IDENTIFIER"

    public void mTokens() throws RecognitionException {
        // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:1:8: ( T__8 | T__9 | WHITESPACE | OPERATOR | INTEGER | IDENTIFIER )
        int alt3=6;
        switch ( input.LA(1) ) {
        case '(':
            {
            alt3=1;
            }
            break;
        case ')':
            {
            alt3=2;
            }
            break;
        case '\t':
        case '\n':
        case '\f':
        case '\r':
        case ' ':
            {
            alt3=3;
            }
            break;
        case '-':
            {
            int LA3_4 = input.LA(2);

            if ( ((LA3_4>='0' && LA3_4<='9')) ) {
                alt3=5;
            }
            else {
                alt3=4;}
            }
            break;
        case '+':
            {
            alt3=4;
            }
            break;
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            {
            alt3=5;
            }
            break;
        case 'A':
        case 'B':
        case 'C':
        case 'D':
        case 'E':
        case 'F':
        case 'G':
        case 'H':
        case 'I':
        case 'J':
        case 'K':
        case 'L':
        case 'M':
        case 'N':
        case 'O':
        case 'P':
        case 'Q':
        case 'R':
        case 'S':
        case 'T':
        case 'U':
        case 'V':
        case 'W':
        case 'X':
        case 'Y':
        case 'Z':
        case '_':
        case 'a':
        case 'b':
        case 'c':
        case 'd':
        case 'e':
        case 'f':
        case 'g':
        case 'h':
        case 'i':
        case 'j':
        case 'k':
        case 'l':
        case 'm':
        case 'n':
        case 'o':
        case 'p':
        case 'q':
        case 'r':
        case 's':
        case 't':
        case 'u':
        case 'v':
        case 'w':
        case 'x':
        case 'y':
        case 'z':
            {
            alt3=6;
            }
            break;
        default:
            NoViableAltException nvae =
                new NoViableAltException("", 3, 0, input);

            throw nvae;
        }

        switch (alt3) {
            case 1 :
                // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:1:10: T__8
                {
                mT__8(); 

                }
                break;
            case 2 :
                // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:1:15: T__9
                {
                mT__9(); 

                }
                break;
            case 3 :
                // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:1:20: WHITESPACE
                {
                mWHITESPACE(); 

                }
                break;
            case 4 :
                // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:1:31: OPERATOR
                {
                mOPERATOR(); 

                }
                break;
            case 5 :
                // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:1:40: INTEGER
                {
                mINTEGER(); 

                }
                break;
            case 6 :
                // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:1:48: IDENTIFIER
                {
                mIDENTIFIER(); 

                }
                break;

        }

    }


 

}