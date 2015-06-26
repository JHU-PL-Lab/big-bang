(*
 * TinyBang - Simple Logging
 *
 * Features
 * --------
 *
 * 1. Always logs to the stderr, which is the best thing to do [1]. Note that the
 *    article in [1] says to log to stdout, but currently the interpreter is only
 *    run in interactive mode, so stdout is already taken and we log the next best
 *    thing, which is stderr.
 *
 * 2. Handles different log levels.
 *
 * 3. Is contextualized by the module and different log levels per module can
 *    be configured.
 *
 * 4. Is a thin layer around BatLog.
 *
 * Usage
 * -----
 *
 * 1. In `_oasis`, for the library that requires logging, add `tiny-bang-utils` to
 *    the `BuildDepends` section.
 *
 * 2. In the module that needs logging, on the top of the file, create a logger
 *    with a name that identifies the module. For example, in `tiny_bang_toploop.ml`:
 *
 *    ```ocaml
 *    let logger = Tiny_bang_logger.make_logger "Tiny_bang_toploop"
 *    ```
 *
 * 3. Add log entries. For example:
 *
 *    ```ocaml
 *    logger `debug "chunky tempeh!"
 *    ```
 *
 *    Allowed log levels are: `trace | `debug | `info | `warn | `error
 *                            | `fatal | `always.
 *
 * 4. (Optional) Log levels can be selected for the entire executable or on a
 *               per-module basis using command line arguments. Refer to the
 *               `README.md' for more information.
 *
 *
 * [1]: http://12factor.net/logs
 *)

open Batteries

let match_string_with_level level_string = 
  match level_string with
     | "trace" -> `trace
     | "debug" -> `debug
     | "info" -> `info
     | "warn" -> `warn
     | "error" -> `error
     | "fatal" -> `fatal
     | "always" -> `always
     | _ -> failwith ("Invalid log level `" ^ level_string ^ "'.")
;;

let default_level = ref `warn ;;

let parse_command_line_parameters result = 
  let parser = BatOptParse.OptParser.make ~version:"version 2.0" () in
  let option_log = BatOptParse.StdOpt.str_option ~default:"warn" () in
  BatOptParse.OptParser.add parser ~help:"logging level" ~long_name:"log" option_log;
  BatOptParse.OptParser.parse_argv parser;
  let parsed_string = BatOptParse.Opt.get option_log in
  if BatString.exists parsed_string "=" then
    let (module_name, level_string) = BatString.split parsed_string "=" in
                                  BatMap.add module_name level_string result
  else
    let () = default_level := match_string_with_level parsed_string in
    result
;;

let level_map = ref (parse_command_line_parameters(BatMap.empty));;

let level_for prefix =
  if BatMap.mem prefix !level_map
    then match_string_with_level (BatMap.find prefix !level_map)
  else
    !default_level
;;

let make_logger prefix level message =
  BatLog.Easy.level := level_for prefix;
  BatLog.Easy.log level ("[" ^ prefix ^ "]: " ^ message);
  flush stdout
;;

let bracket_log logger level pre_message post_message_fn thunk =
  logger level pre_message;
  let value = thunk () in
  logger level (pre_message ^ "\n  : " ^ post_message_fn value);
  value
;;
