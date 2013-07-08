# EXPECT: 8
x1 = 3;
x2 = 3;
p1 = () <- { `True z1 };
f1 = () -> { r1 = 8; };
p2 = () <- { `False z1 };
f2 = () -> { r2 = 7; };
s1 = p1 >< f1;
s2 = p2 >< f2;
on = s1 & s2;
x3 = x1 == x2;
r = on x3
