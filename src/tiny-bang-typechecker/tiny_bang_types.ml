open Batteries;;

open Tiny_bang_ast;;
open Tiny_bang_contours_types;;

(* ************************************************************************** *)
(* BASIC DATA TYPES *)
(* The simple, largely non-recursive data types. *) 
  
type tvar =
  | Tvar of ident * contour option
;;

module Tvar_order =
struct
  type t = tvar
  let compare = compare 
end;;

module Tvar_set = Set.Make(Tvar_order);;

module Tvar_map = Map.Make(Tvar_order);;

type pattern_filter_type =
  | Empty_filter_type
  | Label_filter_type of label * tvar
  | Conjunction_filter_type of tvar * tvar
;;
  
type pattern_type =
  | Pattern_type of tvar * pattern_filter_type Tvar_map.t
;;

module Pattern_type_order =
struct
  type t = pattern_type
  let compare = compare
end;;

module Pattern_type_set = Set.Make(Pattern_type_order);;

(* ************************************************************************** *)
(* TYPE INTERFACE *)
(* This section contains the module type interfaces used to define the
   types representing the TinyBang type system.  We wish to use a sophisticated
   constraint database (rather than linear searches); OCaml's module system
   makes this difficult in conjunction with recursive modules.  We address this
   by defining module interfaces with open type parameters which we can connect
   later during implementation.
*)

(** The signature of modules providing constraint database behavior. *)
module type Constraint_database_sig =
sig
  (** The type of the constraint database. *)
  type t
  (** The type of sets of constraints. *)
  type tbconstraint_set
  (** The type of the constraint that the database stores. *)
  type tbconstraint
  (** The type of lower bounds in the constraints in this database. *)
  type lower_bound
  (** The type of filtered types in this database. *)
  type filtered_type
  (** The type representing TinyBang types in this database. *)
  type tbtype
  (** Creates a constraint database from a set of constraints. *)
  val of_set : tbconstraint_set -> t
  (** Creates a constraint database from an enumeration of constraints. *)
  val of_enum : tbconstraint Enum.t -> t
  (** Extracts all of the constraints from a constraint database. *)
  val to_set : t -> tbconstraint_set
  (** The empty constraint database. *)
  val empty : t
  (** Enumerates over all constraints in this database. *)
  val enum : t -> tbconstraint Enum.t
  (** Adds a constraint to this database. *)
  val add : tbconstraint -> t -> t
  (** Unions two constraint sets. *)
  val union : t -> t -> t
  (** Retrieves all type lower bounds of the provided type variable. *)
  val type_lower_bounds_of : tvar -> t -> filtered_type Enum.t
  (** Determines all of the type variables bound by a constraint database. *)
  val bound_variables_of : t -> Tvar_set.t
end;;

(** The signature of modules providing basic TinyBang type structures.  This is
    usefully separate because it prevents duplication of type declarations in
    concrete implementations of constraint databases. *)
module type Types_basis =
sig
  (** The type of constraint databases for this incarnation of TinyBang
      types. *)
  type tbconstraint_database
  (** The type of constraints in TinyBang. *)
  type tbconstraint = Constraint of lower_bound * tvar
  (** The lower bounds appearing on constraints. *)
  and lower_bound =
    | Type_lower_bound of filtered_type
    | Intermediate_lower_bound of tvar
    | Application_lower_bound of tvar * tvar
  (** A variant representing filtered types. *)
  and filtered_type =
    | Filtered_type of tbtype * Pattern_type_set.t * Pattern_type_set.t
  (** A variant representing shallow TinyBang types. *)
  and tbtype =
    | Empty_onion_type
    | Label_type of label * tvar
    | Onion_type of tvar * tvar
    | Function_type of pattern_type * tvar * tbconstraint_database  
end;;

(* ************************************************************************** *)
(* TYPE IMPLEMENTATION *)
(* The implementation of TinyBang types currently used by the system.  This
   implementation need not be unique; in theory, the below approach could be
   used to create e.g. a different constraint database implementation.  Indeed,
   one might refactor the system to allow the selection of an implementation at
   runtime.
*)

module rec Constraint_ord : Set.OrderedType =
struct
  type t = Types.tbconstraint
  let compare = compare
end

and Constraint_set : (Set.S with type elt = Types.tbconstraint) = Set.Make(Constraint_ord)

and Types : Types_basis
              with type tbconstraint_database := Constraint_database.t
  = Types

and Constraint_database :
      Constraint_database_sig
        with type tbconstraint := Types.tbconstraint
         and type tbconstraint_set := Constraint_set.t
         and type tbtype := Types.tbtype
         and type filtered_type := Types.filtered_type
         and type lower_bound := Types.lower_bound
  =
struct
  include Types
  (* TODO: pick a more sophisticated data structure for performance. *)
  type t =
    | Constraint_database_impl of Constraint_set.t;;

  let of_set cs = Constraint_database_impl cs;;

  let of_enum cs = Constraint_database_impl(Constraint_set.of_enum cs);;

  let to_set (Constraint_database_impl cs) = cs;;

  let empty = Constraint_database_impl (Constraint_set.empty);;

  let enum db = Constraint_set.enum @@ to_set db;;

  let add c (Constraint_database_impl cs) =
    Constraint_database_impl(Constraint_set.add c cs);;

  let union (Constraint_database_impl cs1) (Constraint_database_impl cs2) =
    Constraint_database_impl(Constraint_set.union cs1 cs2);;

  let type_lower_bounds_of a (Constraint_database_impl cs) =
    cs
      |> Constraint_set.enum
      |> Enum.filter_map
          (fun (Constraint(lb,a')) ->
            match lb with
              | Type_lower_bound(rt) ->
                  if a = a' then Some rt else None
              | _ ->
                  None
          )
  ;;

  let bound_variables_of (Constraint_database_impl cs) =
    cs
      |> Constraint_set.enum
      |> Enum.map (fun (Constraint(_,a)) -> a)
      |> Tvar_set.of_enum
  ;;
end;;

(* ************************************************************************** *)
(* TYPE EXPOSURE *)
(* This section ensures that users of this module need not indirect through
   their module namespaces unnecessarily.
*)
include Types;;
