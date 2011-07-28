module Language.BigBang.Pretty.PrettyPrint
( pretty
) where

import Language.BigBang.Ast
import Text.PrettyPrint
import Language.BigBang.Types.UtilTypes (unIdent, unLabelName)

ti i = text $ unIdent i
tl l = text $ unLabelName l
pp e = parens $ pretty e
pp2 t a b = text t <+> pp a <+> pp b

pretty (Var x) = ti x

pretty (Label l e) = char '`' <> tl l <+> pp e

pretty (Onion a b) = pp a <+> pp b

pretty (Func i e) = text "fun" <+> ti i <+> pp e

pretty (Appl a b) = pp a <+> pp b

pretty (PrimInt i) = integer i

pretty (PrimChar c) = char c

pretty (PrimUnit) = parens empty

pretty (Plus a b) = pp2 "plus" a b

pretty (Minus a b) = pp2 "minus" a b

pretty (Equal a b) = pp2 "equal" a b
