# EXPECT: typefail
p1 = (x) <- { x & int };
f1 = (y) -> { yr = y };
s1 = p1 >< f1;
x1 = ();
r = s1 x2
