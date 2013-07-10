# EXPECT: `True ()

isListP = () <- { rec p: `Nil _ | (`Hd `Dollars _ & `Tl p) };
anyP = () <- { z };
trueF = () -> { xe1 = (); xr1 = `True xe1 };
falseF = () -> { xe2 = (); xr2 = `False xe2 };
trueS = isListP >< trueF;
falseS = anyP >< falseF;
cond = falseS & trueS;

x1 = ();
x2 = `Nil x1;

x3 = 4;
x3p = `Dollars x3;
x4 = `Hd x3p;
x5 = `Tl x2;
x6 = x4 & x5;

x7 = 4;
x7p = `Dollars x7;
x8 = `Hd x7p;
x9 = `Tl x6;
x10 = x8 & x9;

ans = cond x10;
