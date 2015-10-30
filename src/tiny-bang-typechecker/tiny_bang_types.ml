open Batteries;;

open Tiny_bang_ast;;
open Tiny_bang_contours_types;;

(* ************************************************************************** *)
(** {6 Basic data types} *)
(** The simple, largely non-recursive data types. *) 

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

type tpvar =
  | Tpvar of ident
;;

let tvar_of_tpvar (Tpvar(i)) = Tvar(i,None);;

module Tpvar_order =
struct
  type t = tpvar
  let compare = compare 
end;;

module Tpvar_set = Set.Make(Tpvar_order);;

module Tpvar_map = Map.Make(Tpvar_order);;

type pattern_filter_type =
  | Empty_filter_type
  | Label_filter_type of label * tpvar
  | Conjunction_filter_type of tpvar * tpvar
  | Int_filter_type of tpvar
  | Array_filter_type of tpvar
  | Ref_filter_type of tpvar
;;

type pattern_type =
  | Pattern_type of tpvar * pattern_filter_type Tpvar_map.t
;;

module Pattern_type_order =
struct
  type t = pattern_type
  let compare = compare
end;;

module Pattern_type_set = Set.Make(Pattern_type_order);;

(* ************************************************************************** *)
(** {6 Type interface} *)
(**
   This section contains the module type interfaces used to define the
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

  (** Determines the size of a constraint database. *)
  val size : t -> int

  (** Enumerates over all constraints in a database. *)
  val enum : t -> tbconstraint Enum.t

  (** Adds a constraint to this database. *)
  val add : tbconstraint -> t -> t

  (** Unions two constraint sets. *)
  val union : t -> t -> t

  (** Retrieves all type lower bounds of the provided type variable. *)
  val type_lower_bounds_of : tvar -> t -> filtered_type Enum.t

  (** Determines all of the type variables bound by a constraint database. *)
  val bound_variables_of : t -> Tvar_set.t  

  (** Replaces type variables appearing in this constraint database. *)
  val replace_variables : (tvar -> tvar) -> t -> t
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
  type tbconstraint =
    | Lower_bound_constraint of lower_bound * tvar
    | Inconsistency_constraint (* TODO: payload describing why! *)

  (** The lower bounds appearing on constraints. *)
  and lower_bound =
    | Type_lower_bound of filtered_type
    | Intermediate_lower_bound of tvar
    | Application_lower_bound of tvar * tvar
    | Builtin_lower_bound of builtin_op * tvar list

  (** A variant representing filtered types. *)
  and filtered_type =
    | Filtered_type of tbtype * Pattern_type_set.t * Pattern_type_set.t

  (** A variant representing shallow TinyBang types. *)
  and tbtype =
    | Empty_onion_type
    | Label_type of label * tvar
    | Onion_type of tvar * tvar
    | Function_type of pattern_type * tvar * tbconstraint_database
    | Ref_type of tvar
    | Array_type of tvar
    | Int_type 
end;;

(* ************************************************************************** *)
(** {6 Partial type implementation} *)
(**
   This section defines an abstract implementation of functions which operate
   on types.  It is as complete as possible without knowing the concrete type
   of the constraint database.
*)

(**
   The signature of a module which provides generic functionality for an abstract
   type module.
*)
module type Type_functions_sig =
sig
  type tbtype
  type filtered_type
  type lower_bound
  type tbconstraint

  (**
     Replaces the type variables appearing in a type.
     @param f The type variable replacement function.
     @param t The type in which to replace.
     @return The resulting type.
  *)
  val replace_vars_of_type : (tvar -> tvar) -> tbtype -> tbtype

  (**
     Replaces the type variables appearing in a filtered type.
     @param f The type variable replacement function.
     @param ft The filtered type in which to replace.
     @return The resulting filtered type.
  *)
  val replace_vars_of_filtered_type :
    (tvar -> tvar) -> filtered_type -> filtered_type

  (**
     Replaces type variables appearing in constraint lower bounds.
     @param f The type variable replacement function.
     @param lb The lower bound in which to perform replacemen
     @return The resulting lower bound.    
  *)
  val replace_vars_of_lower_bound :
    (tvar -> tvar) -> lower_bound -> lower_bound

  (**
     Replaces type variables appearing within a constrain
     @param f The type variable replacement function.
     @param c The constraint in which to perform replacemen
     @return The resulting constrain
  *)
  val replace_vars_of_constraint :
    (tvar -> tvar) -> tbconstraint -> tbconstraint
  
  (**
     Produces a filtered type from a type; the filtered type is essentially
     unfiltered in that its filter sets are empty.
     @param t The raw type.
     @return The equivalent filtered type.
  *)
  val unfiltered_type : tbtype -> filtered_type
end;;

(** A functor which produces abstract functionality for an abstract type module
    and a constraint database module. *)
module Type_functions_functor
    (T : Types_basis)
    (Db : Constraint_database_sig
     with type t = T.tbconstraint_database)
  : Type_functions_sig with type tbconstraint = T.tbconstraint
                        and type lower_bound = T.lower_bound
                        and type filtered_type = T.filtered_type
                        and type tbtype = T.tbtype
=
struct
  type tbconstraint = T.tbconstraint
  type lower_bound = T.lower_bound
  type filtered_type = T.filtered_type
  type tbtype = T.tbtype

  let replace_vars_of_type f t =
    match t with
    | T.Empty_onion_type -> t
    | T.Int_type -> t
    | T.Label_type(l,a) -> T.Label_type(l, f a)
    | T.Onion_type(a1,a2) -> T.Onion_type(f a1, f a2)
    | T.Ref_type(a1) -> T.Ref_type(f a1)
    | T.Array_type(a1) -> T.Array_type(f a1)
    | T.Function_type(pt,a,cs) ->
      (* Pattern types are never subject to variable replacement. *)
      T.Function_type(pt, f a, Db.replace_variables f cs)
  ;;

  let replace_vars_of_filtered_type f (T.Filtered_type(t,pp,pn)) =
    let t' = replace_vars_of_type f t in
    T.Filtered_type(t', pp, pn)
  ;;

  let replace_vars_of_lower_bound f lb =
    match lb with
    | T.Type_lower_bound(ft) ->
      T.Type_lower_bound(replace_vars_of_filtered_type f ft)
    | T.Intermediate_lower_bound(a) ->
      T.Intermediate_lower_bound(f a)
    | T.Application_lower_bound(a1, a2) ->
      T.Application_lower_bound(f a1, f a2)
    | T.Builtin_lower_bound(op, a_list) ->
      T.Builtin_lower_bound(op, List.map f a_list)
  ;;

  let replace_vars_of_constraint f c =
    match c with
    | T.Lower_bound_constraint(lb,a) ->
      T.Lower_bound_constraint(replace_vars_of_lower_bound f lb, f a)
    | T.Inconsistency_constraint ->
      c
  ;;

  let unfiltered_type t =
    T.Filtered_type(t,Pattern_type_set.empty,Pattern_type_set.empty)
  ;;
end;;

(* ************************************************************************** *)
(** {6 Full type implementation} *)
(**
   The implementation of TinyBang types currently used by the system.  This
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

and Constraint_set : (Set.S with type elt = Types.tbconstraint)
  = Set.Make(Constraint_ord)

and Types : Types_basis
  with type tbconstraint_database = Constraint_database.t
  = Types

and Constraint_database :
  Constraint_database_sig
  with type tbconstraint = Types.tbconstraint
   and type tbtype = Types.tbtype
   and type filtered_type = Types.filtered_type
   and type lower_bound = Types.lower_bound
=
struct
  include Types
  type tbconstraint_set = Constraint_set.t
  (* TODO: pick a more sophisticated data structure for performance. *)
  type t =
    | Constraint_database_impl of Constraint_set.t;;

  let of_set cs = Constraint_database_impl cs;;

  let of_enum cs = Constraint_database_impl(Constraint_set.of_enum cs);;

  let to_set (Constraint_database_impl cs) = cs;;

  let empty = Constraint_database_impl (Constraint_set.empty);;

  let size (Constraint_database_impl cs) = Constraint_set.cardinal cs;;

  let enum db = Constraint_set.enum @@ to_set db;;

  let add c (Constraint_database_impl cs) =
    Constraint_database_impl(Constraint_set.add c cs);;

  let union (Constraint_database_impl cs1) (Constraint_database_impl cs2) =
    Constraint_database_impl(Constraint_set.union cs1 cs2);;

  let type_lower_bounds_of a (Constraint_database_impl cs) =
    cs
    |> Constraint_set.enum
    |> Enum.filter_map
      (fun c -> match c with
         | Lower_bound_constraint(lb,a') ->
           begin
             match lb with
             | Type_lower_bound(rt) ->
               if a = a' then Some rt else None
             | _ ->
               None
           end
         | Inconsistency_constraint -> None
      )
  ;;

  let bound_variables_of (Constraint_database_impl cs) =
    cs
    |> Constraint_set.enum
    |> Enum.filter_map
      (fun c -> match c with
         | Lower_bound_constraint(_,a) -> Some a
         | Inconsistency_constraint -> None)
    |> Tvar_set.of_enum
  ;;

  let replace_variables f (Constraint_database_impl cs) =
    let cs' =
      cs
      |> Constraint_set.map (Type_functions.replace_vars_of_constraint f)
    in
    Constraint_database_impl cs'
  ;;
end

and Type_functions : Type_functions_sig
  with type tbconstraint = Types.tbconstraint
   and type lower_bound = Types.lower_bound
   and type filtered_type = Types.filtered_type
   and type tbtype = Types.tbtype
  = Type_functions_functor(Types)(Constraint_database)
;;

(* ************************************************************************** *)
(* TYPE EXPOSURE *)
(* This section ensures that users of this module need not indirect through
   their module namespaces unnecessarily.
*)
include Types;;
let unfiltered_type = Type_functions.unfiltered_type;;
