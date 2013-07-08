# EXPECT: 4

x1 = 4;
x2 = `A x1;
x3 = 5;
x4 = `B x3;
x5 = x2 & x4;
p1 = (n) <- { `A n };
f1 = (x) -> { xr = x };
s1 = p1 >< f1;
r = s1 x5
