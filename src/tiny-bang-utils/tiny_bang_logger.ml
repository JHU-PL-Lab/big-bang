(*
 * TinyBang - Simple Logging
 *
 * Features
 * --------
 *
 * 1. Always logs to the stderr, which is the best thing to do [1].
 *
 * 2. Handles different log levels.
 *
 * 3. Is contextualized by the module.
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
 * 4. (Optional) Adjust the log level. The default is `always, to set it to `debug,
 *    for example:
 *
 *    ```ocaml
 *    Tiny_bang_logger.level `trace
 *    ```
 *
 *    This is a global level that applies to all loggers. You may wish to change
 *    it in the start of the application or selectively for your module, by later
 *    reseting the level.
 *
 * ---
 *
 * WARNING: Be aware that, due to I/O buffering, log entries may not show up on
 *          the terminal as soon as you might expect. To solve that, you might want
 *          to call `flush stdout'.
 *
 * [1]: http://12factor.net/logs
 *)

open Batteries

let default_level =
  let rec parse_command_line_parameters arguments result =
    match arguments with
    | [] -> result
    | [single_parameter] -> if single_parameter = "--log" then
                              failwith "Missing log level after `--log'."
                            else
                              result
    | flag :: argument :: arguments_tail ->
       if flag = "--log" then
         if BatString.exists argument "=" then
           result
         else
           argument
       else
         parse_command_line_parameters (argument :: arguments_tail) result
  in
  parse_command_line_parameters (BatArray.to_list Sys.argv) "warn"
;;
let level_map =
  let rec parse_command_line_parameters arguments result =
    match arguments with
    | [] -> result
    | [single_parameter] -> if single_parameter = "--log" then
                              failwith "Missing log level after `--log'."
                            else
                              result
    | flag :: argument :: arguments_tail ->
       if flag = "--log" then
         if BatString.exists argument "=" then
           let (module_name, level_string) = BatString.split argument "=" in
           BatMap.add module_name level_string result
         else
           result
       else
         parse_command_line_parameters (argument :: arguments_tail) result
  in
  parse_command_line_parameters (BatArray.to_list Sys.argv) BatMap.empty
;;

let level_for prefix =
  let level_string =
    if BatMap.mem prefix level_map then
      BatMap.find prefix level_map
    else
      default_level
  in match level_string with
     | "trace" -> `trace
     | "debug" -> `debug
     | "info" -> `info
     | "warn" -> `warn
     | "error" -> `error
     | "fatal" -> `fatal
     | "always" -> `always
     | _ -> failwith ("Invalid log level `" ^ level_string ^ "'.")
;;

let make_logger prefix level message =
  BatLog.Easy.level := level_for prefix;
  BatLog.Easy.log level ("[" ^ prefix ^ "]: " ^ message)
;;

let bracket_log logger level pre_message post_message_fn thunk =
  logger level pre_message;
  let value = thunk () in
  logger level (pre_message ^ "\n  : " ^ post_message_fn value);
  value
;;
