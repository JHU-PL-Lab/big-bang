# EXPECT: `False ()

isListP = () <- { rec p: `Nil _ | (`Hd _ & `Tl p) };
anyP = () <- { z };
trueF = () -> { xe1 = (); xr1 = `True xe1 };
falseF = () -> { xe2 = (); xr2 = `False xe2 };
trueS = isListP >< trueF;
falseS = anyP >< falseF;
cond = falseS & trueS;

x1 = ();
x2 = x1;

x3 = 4;
x4 = `Hd x3;
x5 = `Tl x2;
x6 = x4 & x5;

x7 = 4;
x8 = `Hd x7;
x9 = `Tl x6;
x10 = x8 & x9;

ans = cond x10;
