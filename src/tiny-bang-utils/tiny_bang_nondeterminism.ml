(**
  This module contains a non-determinism monad.
*)

open Batteries;;
open Tiny_bang_utils;;

module type Nondeterminism_monad_sig = sig
  include Interfaces.Monad
  
  val enum_choose : 'a Enum.t -> 'a m;;

  val list_choose : 'a list -> 'a m;;

  val enum : 'a m -> 'a Enum.t;;
end;;

module Nondeterminism_monad : Nondeterminism_monad_sig = struct
  type 'a m = 'a Enum.t;;

  let return = Enum.singleton;;

  let bind m k = Enum.concat (Enum.map k m);;

  let enum_choose e = e;;

  let list_choose l = List.enum l;;

  let enum e = e;;
end;;
