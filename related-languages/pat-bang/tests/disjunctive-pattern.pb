# EXPECT: 4

p = () <- { int | `A _ };
f = () -> { xr = 4 };
s = p >< f;
a = 8;
r = s a
