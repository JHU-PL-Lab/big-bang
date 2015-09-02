open Tiny_bang_ast_uid;;
open Tiny_bang_source_origin;;
open Lexing;;

let ast_position_hash : file_region Ast_uid_hashtbl.t ref
  = ref (Ast_uid_hashtbl.create(10))
;;

let reset_ast_position_hash () =
  ast_position_hash := Ast_uid_hashtbl.create(10)
;;

let get_ast_position_hash () =
  !ast_position_hash
;;

let next_uid startpos endpos =
  let uid : ast_uid = Tiny_bang_ast_uid.next_uid () in
  let start = { file_pos_lineno = startpos.pos_lnum
              ; file_pos_colno = startpos.pos_bol
              } in
  let stop = { file_pos_lineno = endpos.pos_lnum
             ; file_pos_colno = endpos.pos_bol
             } in
  let region = { file_region_filename = startpos.pos_fname
               ; file_region_start = start
               ; file_region_end = stop
               } in
  Ast_uid_hashtbl.add (get_ast_position_hash()) uid region;
  uid
;;
