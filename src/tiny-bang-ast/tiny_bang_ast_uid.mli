(**
  Provides support mechanisms for the AST module.  This module should not be
  used directly; the Tiny_bang_ast module should be used instead.
*)

(**/**)

open Batteries;;

type ast_uid

val next_uid : unit -> ast_uid

module Ast_uid_hashtbl : Hashtbl.S with type key = ast_uid
