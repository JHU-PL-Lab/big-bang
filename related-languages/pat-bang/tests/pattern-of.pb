# EXPECT: 8

x1 = 4;
x2 = `A x1;
x3 = 8;
x4 = `B x3;
x5 = x2 & x4;

pA = (n) <- { `A n };
fid = (x) -> { xr = x; };
sA = pA >< fid;
pAB = (m) <- { `B m & $sA };
sAB = pAB >< fid;

r = sAB x5;

