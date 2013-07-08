# EXPECT: typefail
p1 = () <- { z };
f1 = (x1) -> {
    p2 = () <- { z1 };
    f2 = (x2) -> {
            r = x1 + x2
        };
    s2 = p2 >< f2;
    };
s1 = p1 >< f1;
x3 = 3;
x4 = ();
x5 = s1 x3;
x6 = x5 x4
