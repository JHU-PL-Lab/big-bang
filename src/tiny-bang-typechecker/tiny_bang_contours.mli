open Batteries;;

open Tiny_bang_ast;;
open Tiny_bang_contours_types;;
open Tiny_bang_utils;;

(** Determines whether or not a contour is well-formed. *)
val check_well_formed : contour -> bool

(** Asserts that a contour is well-formed.  If it is not, then
    [Not_yet_implemented] is raised. *)
val assert_well_formed : contour -> unit

(** Determines the least contour greater than the provided contour which is
    well-formed. *)
val derive_least_well_formed : contour -> contour

(** Determines if the first contour subsumes the second.  Both contours must be
    well-formed. *)
val subsumes : contour -> contour -> bool
 
(** Determines if the two contours overlap; that is, returns true when any call
    string exists which is accepted by both contours.  Both contours must be
    well-formed. *)
val overlaps : contour -> contour -> bool
