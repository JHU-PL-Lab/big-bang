# EXPECT: `False ()

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
# *except* for the second-to-last element, which is just an int
buildList0P = (self) <- { self };
buildList0F = (self) -> {
        buildListBaseP = (m) <- { int & m };
        buildListBaseF = (n) -> {
                trueP = () <- { `True _ };
                falseP = () <- { `False _ };
                blBaseF = () -> {
                        blbaseEmpty = ();
                        blbaseFr = `Nil blbaseEmpty;
                    };
                blIndF = () -> {
                        mkElP = () <- { z };
                        mkElF = () -> {
                                dollarsElF = () -> {
                                        dollarsElFr = `Dollars n;
                                    };
                                dollarsElS = falseP >< dollarsElF;
                                numElF = () -> {
                                        numElFr = n;
                                    };
                                numElS = trueP >< numElF;
                                mkElCond = dollarsElS & numElS;
                                two = 2;
                                mkElBool = n == two;
                                mkElRet = mkElCond mkElBool;
                            };
                        mkEl = mkElP >< mkElF;
                        one = 1;
                        nMinus1 = n - one;
                        rest = self nMinus1;
                        tail = `Tl rest;
                        el = mkEl n;
                        ret = el & tail;
                    };
                blBase = trueP >< blBaseF;
                blInd = falseP >< blIndF;
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

# Verify that it's *not* a list of dollars
ans = cond theList;
