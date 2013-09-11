// $ANTLR 3.2 Sep 23, 2009 12:02:23 /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g 2013-09-10 20:57:05

import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

import org.antlr.runtime.tree.*;

public class TestParser extends Parser {
    public static final String[] tokenNames = new String[] {
        "<invalid>", "<EOR>", "<DOWN>", "<UP>", "OPERATOR", "INTEGER", "IDENTIFIER", "WHITESPACE", "'('", "')'"
    };
    public static final int INTEGER=5;
    public static final int IDENTIFIER=6;
    public static final int OPERATOR=4;
    public static final int WHITESPACE=7;
    public static final int EOF=-1;
    public static final int T__9=9;
    public static final int T__8=8;

    // delegates
    // delegators


        public TestParser(TokenStream input) {
            this(input, new RecognizerSharedState());
        }
        public TestParser(TokenStream input, RecognizerSharedState state) {
            super(input, state);
            this.state.ruleMemo = new HashMap[7+1];
             
             
        }
        
    protected TreeAdaptor adaptor = new CommonTreeAdaptor();

    public void setTreeAdaptor(TreeAdaptor adaptor) {
        this.adaptor = adaptor;
    }
    public TreeAdaptor getTreeAdaptor() {
        return adaptor;
    }

    public String[] getTokenNames() { return TestParser.tokenNames; }
    public String getGrammarFileName() { return "/Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g"; }


    public static class program_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "program"
    // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:10:1: program : expression EOF ;
    public final TestParser.program_return program() throws RecognitionException {
        TestParser.program_return retval = new TestParser.program_return();
        retval.start = input.LT(1);
        int program_StartIndex = input.index();
        Object root_0 = null;

        Token EOF2=null;
        TestParser.expression_return expression1 = null;


        Object EOF2_tree=null;

        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 1) ) { return retval; }
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:10:8: ( expression EOF )
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:10:10: expression EOF
            {
            root_0 = (Object)adaptor.nil();

            pushFollow(FOLLOW_expression_in_program50);
            expression1=expression();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) adaptor.addChild(root_0, expression1.getTree());
            EOF2=(Token)match(input,EOF,FOLLOW_EOF_in_program52); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            EOF2_tree = (Object)adaptor.create(EOF2);
            adaptor.addChild(root_0, EOF2_tree);
            }

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 1, program_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "program"

    public static class expression_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "expression"
    // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:12:1: expression : ( primaryExpression OPERATOR expression | primaryExpression );
    public final TestParser.expression_return expression() throws RecognitionException {
        TestParser.expression_return retval = new TestParser.expression_return();
        retval.start = input.LT(1);
        int expression_StartIndex = input.index();
        Object root_0 = null;

        Token OPERATOR4=null;
        TestParser.primaryExpression_return primaryExpression3 = null;

        TestParser.expression_return expression5 = null;

        TestParser.primaryExpression_return primaryExpression6 = null;


        Object OPERATOR4_tree=null;

        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 2) ) { return retval; }
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:13:3: ( primaryExpression OPERATOR expression | primaryExpression )
            int alt1=2;
            switch ( input.LA(1) ) {
            case INTEGER:
                {
                int LA1_1 = input.LA(2);

                if ( (synpred1_Test()) ) {
                    alt1=1;
                }
                else if ( (true) ) {
                    alt1=2;
                }
                else {
                    if (state.backtracking>0) {state.failed=true; return retval;}
                    NoViableAltException nvae =
                        new NoViableAltException("", 1, 1, input);

                    throw nvae;
                }
                }
                break;
            case IDENTIFIER:
                {
                int LA1_2 = input.LA(2);

                if ( (synpred1_Test()) ) {
                    alt1=1;
                }
                else if ( (true) ) {
                    alt1=2;
                }
                else {
                    if (state.backtracking>0) {state.failed=true; return retval;}
                    NoViableAltException nvae =
                        new NoViableAltException("", 1, 2, input);

                    throw nvae;
                }
                }
                break;
            case 8:
                {
                int LA1_3 = input.LA(2);

                if ( (synpred1_Test()) ) {
                    alt1=1;
                }
                else if ( (true) ) {
                    alt1=2;
                }
                else {
                    if (state.backtracking>0) {state.failed=true; return retval;}
                    NoViableAltException nvae =
                        new NoViableAltException("", 1, 3, input);

                    throw nvae;
                }
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 1, 0, input);

                throw nvae;
            }

            switch (alt1) {
                case 1 :
                    // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:14:5: primaryExpression OPERATOR expression
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_primaryExpression_in_expression66);
                    primaryExpression3=primaryExpression();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, primaryExpression3.getTree());
                    OPERATOR4=(Token)match(input,OPERATOR,FOLLOW_OPERATOR_in_expression68); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    OPERATOR4_tree = (Object)adaptor.create(OPERATOR4);
                    adaptor.addChild(root_0, OPERATOR4_tree);
                    }
                    pushFollow(FOLLOW_expression_in_expression70);
                    expression5=expression();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, expression5.getTree());

                    }
                    break;
                case 2 :
                    // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:15:5: primaryExpression
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_primaryExpression_in_expression76);
                    primaryExpression6=primaryExpression();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, primaryExpression6.getTree());

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 2, expression_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "expression"

    public static class primaryExpression_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "primaryExpression"
    // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:18:1: primaryExpression : ( INTEGER | IDENTIFIER | parenthesizedExpression );
    public final TestParser.primaryExpression_return primaryExpression() throws RecognitionException {
        TestParser.primaryExpression_return retval = new TestParser.primaryExpression_return();
        retval.start = input.LT(1);
        int primaryExpression_StartIndex = input.index();
        Object root_0 = null;

        Token INTEGER7=null;
        Token IDENTIFIER8=null;
        TestParser.parenthesizedExpression_return parenthesizedExpression9 = null;


        Object INTEGER7_tree=null;
        Object IDENTIFIER8_tree=null;

        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 3) ) { return retval; }
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:19:3: ( INTEGER | IDENTIFIER | parenthesizedExpression )
            int alt2=3;
            switch ( input.LA(1) ) {
            case INTEGER:
                {
                alt2=1;
                }
                break;
            case IDENTIFIER:
                {
                alt2=2;
                }
                break;
            case 8:
                {
                alt2=3;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return retval;}
                NoViableAltException nvae =
                    new NoViableAltException("", 2, 0, input);

                throw nvae;
            }

            switch (alt2) {
                case 1 :
                    // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:20:3: INTEGER
                    {
                    root_0 = (Object)adaptor.nil();

                    INTEGER7=(Token)match(input,INTEGER,FOLLOW_INTEGER_in_primaryExpression91); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    INTEGER7_tree = (Object)adaptor.create(INTEGER7);
                    adaptor.addChild(root_0, INTEGER7_tree);
                    }

                    }
                    break;
                case 2 :
                    // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:21:5: IDENTIFIER
                    {
                    root_0 = (Object)adaptor.nil();

                    IDENTIFIER8=(Token)match(input,IDENTIFIER,FOLLOW_IDENTIFIER_in_primaryExpression97); if (state.failed) return retval;
                    if ( state.backtracking==0 ) {
                    IDENTIFIER8_tree = (Object)adaptor.create(IDENTIFIER8);
                    adaptor.addChild(root_0, IDENTIFIER8_tree);
                    }

                    }
                    break;
                case 3 :
                    // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:22:5: parenthesizedExpression
                    {
                    root_0 = (Object)adaptor.nil();

                    pushFollow(FOLLOW_parenthesizedExpression_in_primaryExpression103);
                    parenthesizedExpression9=parenthesizedExpression();

                    state._fsp--;
                    if (state.failed) return retval;
                    if ( state.backtracking==0 ) adaptor.addChild(root_0, parenthesizedExpression9.getTree());

                    }
                    break;

            }
            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 3, primaryExpression_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "primaryExpression"

    public static class parenthesizedExpression_return extends ParserRuleReturnScope {
        Object tree;
        public Object getTree() { return tree; }
    };

    // $ANTLR start "parenthesizedExpression"
    // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:25:1: parenthesizedExpression : '(' expression ')' ;
    public final TestParser.parenthesizedExpression_return parenthesizedExpression() throws RecognitionException {
        TestParser.parenthesizedExpression_return retval = new TestParser.parenthesizedExpression_return();
        retval.start = input.LT(1);
        int parenthesizedExpression_StartIndex = input.index();
        Object root_0 = null;

        Token char_literal10=null;
        Token char_literal12=null;
        TestParser.expression_return expression11 = null;


        Object char_literal10_tree=null;
        Object char_literal12_tree=null;

        try {
            if ( state.backtracking>0 && alreadyParsedRule(input, 4) ) { return retval; }
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:26:3: ( '(' expression ')' )
            // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:27:3: '(' expression ')'
            {
            root_0 = (Object)adaptor.nil();

            char_literal10=(Token)match(input,8,FOLLOW_8_in_parenthesizedExpression118); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            char_literal10_tree = (Object)adaptor.create(char_literal10);
            adaptor.addChild(root_0, char_literal10_tree);
            }
            pushFollow(FOLLOW_expression_in_parenthesizedExpression120);
            expression11=expression();

            state._fsp--;
            if (state.failed) return retval;
            if ( state.backtracking==0 ) adaptor.addChild(root_0, expression11.getTree());
            char_literal12=(Token)match(input,9,FOLLOW_9_in_parenthesizedExpression122); if (state.failed) return retval;
            if ( state.backtracking==0 ) {
            char_literal12_tree = (Object)adaptor.create(char_literal12);
            adaptor.addChild(root_0, char_literal12_tree);
            }

            }

            retval.stop = input.LT(-1);

            if ( state.backtracking==0 ) {

            retval.tree = (Object)adaptor.rulePostProcessing(root_0);
            adaptor.setTokenBoundaries(retval.tree, retval.start, retval.stop);
            }
        }
        catch (RecognitionException re) {
            reportError(re);
            recover(input,re);
    	retval.tree = (Object)adaptor.errorNode(input, retval.start, input.LT(-1), re);

        }
        finally {
            if ( state.backtracking>0 ) { memoize(input, 4, parenthesizedExpression_StartIndex); }
        }
        return retval;
    }
    // $ANTLR end "parenthesizedExpression"

    // $ANTLR start synpred1_Test
    public final void synpred1_Test_fragment() throws RecognitionException {   
        // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:14:5: ( primaryExpression OPERATOR expression )
        // /Users/GeE/BigBang/eclipse-plugin/resources/grammar/Test.g:14:5: primaryExpression OPERATOR expression
        {
        pushFollow(FOLLOW_primaryExpression_in_synpred1_Test66);
        primaryExpression();

        state._fsp--;
        if (state.failed) return ;
        match(input,OPERATOR,FOLLOW_OPERATOR_in_synpred1_Test68); if (state.failed) return ;
        pushFollow(FOLLOW_expression_in_synpred1_Test70);
        expression();

        state._fsp--;
        if (state.failed) return ;

        }
    }
    // $ANTLR end synpred1_Test

    // Delegated rules

    public final boolean synpred1_Test() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred1_Test_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }


 

    public static final BitSet FOLLOW_expression_in_program50 = new BitSet(new long[]{0x0000000000000000L});
    public static final BitSet FOLLOW_EOF_in_program52 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_primaryExpression_in_expression66 = new BitSet(new long[]{0x0000000000000010L});
    public static final BitSet FOLLOW_OPERATOR_in_expression68 = new BitSet(new long[]{0x0000000000000160L});
    public static final BitSet FOLLOW_expression_in_expression70 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_primaryExpression_in_expression76 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_INTEGER_in_primaryExpression91 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_IDENTIFIER_in_primaryExpression97 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_parenthesizedExpression_in_primaryExpression103 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_8_in_parenthesizedExpression118 = new BitSet(new long[]{0x0000000000000160L});
    public static final BitSet FOLLOW_expression_in_parenthesizedExpression120 = new BitSet(new long[]{0x0000000000000200L});
    public static final BitSet FOLLOW_9_in_parenthesizedExpression122 = new BitSet(new long[]{0x0000000000000002L});
    public static final BitSet FOLLOW_primaryExpression_in_synpred1_Test66 = new BitSet(new long[]{0x0000000000000010L});
    public static final BitSet FOLLOW_OPERATOR_in_synpred1_Test68 = new BitSet(new long[]{0x0000000000000160L});
    public static final BitSet FOLLOW_expression_in_synpred1_Test70 = new BitSet(new long[]{0x0000000000000002L});

}