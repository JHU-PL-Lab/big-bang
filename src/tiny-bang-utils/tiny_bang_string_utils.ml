open Batteries;;

let concat_sep sep strs =
  Enum.fold
    (fun acc s ->
      acc ^
      (if String.is_empty acc then "" else sep) ^
      s
    ) "" strs
;;

let concat_sep_delim start stop sep strs =
  start ^ concat_sep sep strs ^ stop
;;
