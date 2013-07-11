# EXPECT: `True ()

# Set up the function for the test.
listP = (x) <- { rec p: `Nil _ | (`Hd x & `Tl p) };
dollarsP = () <- { `Dollars _ };
isDollarsListP = () <- { listP (dollarsP ()) };
anyP = () <- { z };
trueF = () -> { xe1 = (); xr1 = `True xe1 };
falseF = () -> { xe2 = (); xr2 = `False xe2 };
trueS = isDollarsListP >< trueF;
falseS = anyP >< falseF;
cond = falseS & trueS;

# Build a Y-combinator
allpat = (z) <- { z };
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

# Build a list-building function; this list will have all `Dollars elements
buildList0P = (self) <- { self };
buildList0F = (self) -> {
        buildListBaseP = (m) <- { int & m };
        buildListBaseF = (n) -> {
                blBaseF = () -> {
                        blbaseEmpty = ();
                        blbaseFr = `Nil blbaseEmpty;
                    };
                blBaseP = () <- { `True _ };
                blIndF = () -> {
                        one = 1;
                        nMinus1 = n - one;
                        rest = self nMinus1;
                        tail = `Tl rest;
                        dollars = `Dollars n;
                        el = `Hd dollars;
                        ret = el & tail;
                    };
                blIndP = () <- { `False _ };
                blBase = blBaseP >< blBaseF;
                blInd = blIndP >< blIndF;
                bl = blBase & blInd;
                zero = 0;
                blBool = n == zero;
                blResult = bl blBool;
            };
        buildListBase = buildListBaseP >< buildListBaseF;
    };
buildList0 = buildList0P >< buildList0F;
buildList = Y buildList0;

# Create a list using that function
size = 8;
theList = buildList size;

# Ensure that it's a list of dollars
ans = cond theList;
