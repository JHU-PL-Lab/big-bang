# EXPECT: 0

p = (a,b,c) <- { `A (a & b) & `B c };
f = (n,m) -> { fr = n - m };
s = p >< f;

x1 = 9;
x2 = `A x1;
x3 = 5;
x4 = `B x3;
x5 = x2 & x4;

r = s x5;
