(**
   This module contains a non-determinism monad.
*)

module Enum = Batteries.Enum;;

module type Nondeterminism_monad_sig = sig
  type 'a m
  include Monad.LazyPlus with type 'a m := 'a m
  (* NOTE: The use of Tiny_bang_monad_utils here to provide a sequence operation
           is a workaround for a bug in moandlib 0.1.  The bug is fixed in
           monadlib 0.2, which is not yet available on OPAM. *)
  include Tiny_bang_monad_utils.Utils with type 'a m := 'a m
  val pick_enum : 'a Enum.t -> 'a m
  val enum : 'a m -> 'a Enum.t

  val stop_unless : bool -> unit m
end;;

module Nondeterminism_monad_base = struct
  include Monad.LazyListM;;
  let enum m = LazyList.enum m;;
end;;

module Nondeterminism_monad : Nondeterminism_monad_sig = struct
  module M = Monad.MakeLazyPlus(Nondeterminism_monad_base);;
  include M;;
  module U = Tiny_bang_monad_utils.Make(Nondeterminism_monad_base);;
  let sequence = U.sequence;;
  let mapM = U.mapM;;
  let pick_enum e =
    let lazy_list_of_enum e =
      LazyList.unfold
        e
        (fun e' -> Batteries.Option.map (fun x -> (x,e')) @@ Enum.get e')
    in
    M.of_llist @@ lazy_list_of_enum e;;
  let enum = Nondeterminism_monad_base.enum;;

  let stop_unless x = if x then return () else zero ();;
end;;
