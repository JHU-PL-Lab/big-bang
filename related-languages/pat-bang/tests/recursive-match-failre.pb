# EXPECT: typefail

# Helpful constants
allpat = (z) <- { z };
patpat = (z) <- { z & pat };
intpat = (z) <- { z & int };
truepat = () <- { `True z1 };
falsepat = () <- { `False z2 };
zero = 0;
one = 1;
somebool = zero == one;

# Y combinator
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

# A function to wrap data in `A under some number of levels
wrapA0F1 = (self) -> {
        wrapA0F2 = (n) -> {
                wrapA0F3 = (data) -> {
                        wrapA0F3TrueF = () -> {
                                wrapA0F3TrueFr = data;
                            };
                        wrapA0F3FalseF = () -> {
                                nMinusOne = n - one;
                                innerData1 = self nMinusOne;
                                innerData = innerData1 data;
                                wrapA0F3FalseFr  = `A innerData;
                            };
                        wrapA0F3True = truepat >< wrapA0F3TrueF;
                        wrapA0F3False = falsepat >< wrapA0F3FalseF;
                        wrapA0F3Cond = wrapA0F3True & wrapA0F3False;
                        wrapA0F3Bool = n == zero;
                        wrapA0F3r = wrapA0F3Cond wrapA0F3Bool;
                    };
                wrapA0S3 = allpat >< wrapA0F3;
            };
        wrapA0S2 = intpat >< wrapA0F2;
    };
wrapA0 = allpat >< wrapA0F1;
wrapA = Y wrapA0;

# Some data wrapped under `A
x1 = 8;
x2 = wrapA x1;
x3 = allpat;
x4 = x2 x3;

### A screwed up pattern match
# Match the pattern at top level
f1 = () -> { f1r = 99; };
s1 = patpat >< f1;
# Match the pattern one level in
p2 = () <- { `A pat };
f2 = () -> { f2r = 98; };
s2 = p2 >< f2;
# And recursively match using the same top level but the *wrong* primitive
f3 = (m) -> { f3r = one + m; };
pr = (y) <- { rec p: `A p | intpat (y) };
s3 = pr >< f3;
# Build up the onion of those scapes
sa = s1 & s2;
s = sa & s3;
# This had better type error.  :)
r = s x4;
