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
 *
 * [1]: http://12factor.net/logs
 *)

open Batteries

let level level =
  BatLog.Easy.level := level


let make_logger prefix level message =
  BatLog.Easy.log level ("[" ^ prefix ^ "]: " ^ message)
