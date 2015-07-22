open Batteries;;

open Tiny_bang_logger;;

val logging_option : logging_config BatOptParse.Opt.t

val disable_typechecking_option : bool BatOptParse.Opt.t