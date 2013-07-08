# EXPECT: 21

allpat = (z) <- { z };
truepat = () <- { `True z1 };
falsepat = () <- { `False z2 };

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

If = (Iself) -> {
        cntrF = (cntr) -> {
                baseF = () -> {
                        baser = 0;
                    };
                indF = () -> {
                        one = 1;
                        cntr2 = cntr - one;
                        recres = Iself cntr2;
                        indr = cntr + recres
                    };
                baseS = truepat >< baseF;
                indS = falsepat >< indF;
                condS = baseS & indS;
                zero = 0;
                bool = cntr == zero;
                cntrr = condS bool
            };
        cntrS = allpat >< cntrF;
    };
I = allpat >< If;

fn = Y I;
start = 6;
ans = fn start
