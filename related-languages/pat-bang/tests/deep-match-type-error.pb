# EXPECT: typefail

p1 = (x1) <- { `A `B x1 };
f1 = (x2) -> { xr = x2 };
s1 = p1 >< f1;
x3 = 0;
x4 = `C x3;
x5 = `A x4;
r = s1 x5
