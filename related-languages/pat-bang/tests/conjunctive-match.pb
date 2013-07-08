# EXPECT: 8

p1 = (x1,x2) <- { `A x1 & `B x2 };
f1 = (x3,x4) -> {
        xr1 = x3 + x4;
    };
s1 = p1 >< f1;
x5 = 5;
x6 = `A x5;
x7 = 3;
x8 = `B x7;
x9 = x6 & x8;
r = s1 x9
