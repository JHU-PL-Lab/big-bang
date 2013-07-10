# EXPECT: `False ()

listP = (x) <- { rec p: `Nil _ | (`Hd x & `Tl p) };
dollarsP = () <- { `Dollars _ };
isDollarsListP = () <- { listP (dollarsP ()) };
anyP = () <- { z };
trueF = () -> { xe1 = (); xr1 = `True xe1 };
falseF = () -> { xe2 = (); xr2 = `False xe2 };
trueS = isDollarsListP >< trueF;
falseS = anyP >< falseF;
cond = falseS & trueS;

x1 = ();
x2 = `Nil x1;

x3 = 4;
x4 = `Hd x3;
x5 = `Tl x2;
x6 = x4 & x5;

x7 = 4;
x7p = `Dollars x7;
x8 = `Hd x7p;
x9 = `Tl x6;
x10 = x8 & x9;

ans = cond x10;
