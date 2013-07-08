# EXPECT: typefail

# Build the `A
x1 = 0;
x2 = `A x1;
# Build the `B
x3 = 1;
x4 = `B x3;
# Get a type-ambiguous boolean
x5 = 3;
x6 = 4;
x7 = x5 == x6;
# Pick the `A or the `B based on the boolean
p1 = () <- { `True z1 };
p2 = () <- { `False z2 };
f1 = () -> { xr1 = x2 };
f2 = () -> { xr2 = x4 };
s1 = p1 >< f1;
s2 = p2 >< f2;
s3 = s1 & s2;
x8 = s3 x7;
# Now try to match the conjunction
p4 = () <- { `A z3 & `B z4 };
f4 = () -> { xr4 = () };
s4 = p4 >< f4;
x9 = s4 x8
