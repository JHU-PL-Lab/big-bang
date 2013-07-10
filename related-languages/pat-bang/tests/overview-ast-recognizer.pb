# EXPECT: `True ()

# First, define some handy patterns
allpat = (z) <- { z };
truepat = () <- { `True z1 };
falsepat = () <- { `False z2 };
nilpat = () <- { `Nil _ };
notnilpat = (next,rest) <- { `Hd next & `Tl rest };

# Next, define the Y-combinator.  (The PatBang type system is powerful enough
# to do this between subtype constraints and call-site polymorphism.)
Yf = (f) -> {
        xf = (x) -> {
                vf = (v) -> {
                        xx = x x;
                        vr = xx v
                    };
                vs = allpat >< vf;
                xr = f vs;
            };
        xs = allpat >< xf;
        Yr = xs xs
    };
Y = allpat >< Yf;

# Now define some AST patterns.  We use an arithmetic AST because this PatBang
# implementation does not include string data types; thus, `Label constructors
# aren't really feasible.
p1 = (r) <- { `Int _ };
p2 = (r) <- { `Negate r };
p3 = (r) <- { `Plus (`L r & `R r) };

# Build the list of patterns
empty = ();
pList0 = `Nil empty;

x1 = `Tl pList0;
x2 = `Hd p3;
pList1 = x1 & x2;

x3 = `Tl pList1;
x4 = `Hd p2;
pList2 = x3 & x4;

x5 = `Tl pList2;
x6 = `Hd p1;
pList = x5 & x6;

# Create the pattern disjunction calculator
disjunctPatBasisF = (self) -> {
        disjunctPatNilF = () -> { disjunctPatNilFRet = (x) <- { none }; };
        disjunctPatNil = nilpat >< disjunctPatNilF;
        disjunctPatNotNilF = (pat1,pats) -> {
                pat2 = self pats;
                pat3 = (x) <- { pat1 (x) | pat2 (x) };
            };
        disjunctPatNotNil = notnilpat >< disjunctPatNotNilF;
        disjunctPatScape = disjunctPatNil & disjunctPatNotNil;
    };
disjunctPatBasis = allpat >< disjunctPatBasisF;
disjunctPat = Y disjunctPatBasis;

# Create the disjunctive pattern for the AST
astPat = disjunctPat pList;

# Create an AST for 1 + 4 + (-2).
ast1 = 1;
ast2 = `Int ast1;
ast3 = 4;
ast4 = `Int ast3;
ast5 = 2;
ast6 = `Negate ast5;
ast7 = `L ast2;
ast8 = `R ast4;
ast9 = ast7 & ast8;
ast10 = `Plus ast9;
ast11 = `L ast10;
ast12 = `R ast6;
ast13 = ast11 & ast12;
ast14 = `Plus ast13;

# Create function bodies for the results.
trueF = () -> { trueFR = `True empty; };
falseF = () -> { falseFR = `False empty; };
trueS = astPat >< trueF;
falseS = allpat >< falseF;
cond = falseS & trueS;

# Recognize the AST!
result = cond ast14;
