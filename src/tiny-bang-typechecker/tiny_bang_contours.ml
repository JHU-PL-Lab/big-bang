open Batteries;;

open Tiny_bang_ast;;

type contour_part =
  | SinglePart of ident
  | MultiPart of IdentSet.t
;;

type contour =
  | Contour of contour_part list
  | NoContour
;;
