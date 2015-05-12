open Batteries;;

open Tiny_bang_ast;;
open Tiny_bang_utils;;

type contour_part =
  | SinglePart of ident
  | MultiPart of Ident_set.t
;;

type contour =
  | Contour of contour_part list
;;

let members_of_part part =
  match part with
  | SinglePart(i) -> Ident_set.singleton i
  | MultiPart(is) -> is
;;

let check_well_formed (Contour(contour_parts)) : bool =
  let disjoint_union a b =
    if Ident_set.is_empty (Ident_set.inter a b)
    then Some (Ident_set.union a b)
    else None
  in
  let rec check_parts seen contour_parts =
    match contour_parts with
    | [] -> true
    | (MultiPart(_))::(MultiPart(_))::_ -> false
    | part::t ->
      begin
        match disjoint_union seen (members_of_part part) with
        | Some(seen') -> check_parts seen' t
        | None -> false
      end
  in
  check_parts Ident_set.empty contour_parts
;;

let derive_well_formed (Contour(original_contour_parts)) : contour =
  (* Our first step merges duplicate elements. *)
  let merge_duplicates contour_parts =
    (* We'll start by building a hashtable counting the number of occurrences of
       each ident. We will pair this with a boolean (starting at false), the
       use of which is described below. *)
    let ident_hash = Ident_hashtbl.create(10) in
    let add_elements (part : contour_part) : unit =
      Ident_set.iter
        (fun ident ->
           let (n, b) = Ident_hashtbl.find_default ident_hash ident (0, false)
           in Ident_hashtbl.add ident_hash ident (n +1, b))
        (members_of_part part)
    in
    List.iter add_elements contour_parts;
    (* Next, we will iterate over the parts of the contour. As we reach each
       identifier in the contour, we will subtract an occurrence from the
       hashtable above. If the occurrence count is zero, the entry is deleted;
       otherwise, the associated boolean is set to true to indicate that the
       identifier in question is "open" and all parts should be merged until
       that count is zero. Thus, if any of the pairs in the hashtable are true,
       we should keep merging contour parts. We track the number of such true
       elements in a ref to avoid walking the hashtable at each step. *)
    let open_count = ref 0 in
    let uncount_ident i =
      let (n, b) = Ident_hashtbl.find ident_hash i in
      match (n, b) with
      | (1, false) ->
        (* This is the only occurrence.  Just delete it. *)
        Ident_hashtbl.remove ident_hash i
      | (1, true) ->
        (* This is the last occurrence. Remove it and decrease the open
           count. *)
        Ident_hashtbl.remove ident_hash i;
        open_count := !open_count - 1
      | (_, false) ->
        (* This is the first occurrence. Increase the open count and update
           the entry. *)
        open_count := !open_count + 1;
        Ident_hashtbl.replace ident_hash i (n -1, true)
      | (_, true) ->
        (* This is neither the first nor the last occurrence. Just update
           the entry. *)
        Ident_hashtbl.replace ident_hash i (n -1, true)
    in
    let rec digest_parts parts gathered_set =
      match parts with
      | [] -> []
      | part::parts' ->
        let part_idents = members_of_part part in
        (* Uncount each identifier in the part. *)
        Ident_set.iter uncount_ident part_idents;
        if !open_count > 0 then
          (* There are open identifiers. We need to add all of these to the
             gathered set and keep moving. *)
          digest_parts parts' (Ident_set.union gathered_set part_idents)
        else
          let front_part =
            if not (Ident_set.is_empty gathered_set) then
              (* There are no open identifiers, but we just finished collapsing
                 some. Emit the appropriate MultiSet before continuing. *)
              (MultiPart(Ident_set.union gathered_set part_idents))
            else
              (* There are no open identifiers and everything is normal! *)
              part
          in
          front_part::(digest_parts parts Ident_set.empty)
    in
    digest_parts contour_parts Ident_set.empty
  in
  (* Our next step merges adjacent sets. *)
  let rec merge_adjacent contour_parts =
    match contour_parts with
    | [] -> []
    | part::[] -> part::[]
    | (MultiPart(is1))::(MultiPart(is2))::contour_parts' ->
      merge_adjacent ((MultiPart(Ident_set.union is1 is2))::contour_parts')
    | part1::part2::contour_parts' ->
      part1::(merge_adjacent (part2::contour_parts'))
  in
  (* Now do the work! *)
  Contour
    (original_contour_parts
     |> merge_duplicates
     |> merge_adjacent)
;;
