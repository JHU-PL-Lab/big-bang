open Tiny_bang_ast;;

type contour_part =
  | Single_part of ident
  | Multi_part of Ident_set.t
;;

type contour =
  | Contour of contour_part list
;;

(** Raised when an ill-formed contour is used where a well-formed contour is
    required. *)
exception Ill_formed_contour of contour;;

