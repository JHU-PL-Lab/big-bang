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

(** Raised when an ill-formed contour is used where a well-formed contour is
    required. *)
exception Ill_formed_contour of contour;;

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

let is_all_multi_parts parts =
  parts
  |> List.map (fun part -> match part with
                            | MultiPart(_) -> true
                            | _ -> false)
  |> List.fold_left (&&) true
;;

let rec subsumes_by_parts contour_parts_1 contour_parts_2 =
  match contour_parts_1,contour_parts_2 with
    | [],[] -> true
    | [],_ ->
        (* Then contour 2 has more parts than contour 1.  Regardless of what
           those parts are, there exists some string that contour 2 accepts and
           contour 1 does not. *)
        false
    | _,[] ->
        (* Then contour 1 has more parts than contour 2.  If any of those parts
           are single, however, then contour 1 cannot accept any string from
           contour 2. *)
        is_all_multi_parts contour_parts_1
    | part_1::contour_parts_1',part_2::contour_parts_2' ->
        match part_1,part_2 with
          | SinglePart(i1),SinglePart(i2) ->
              (* In this case, both contours require that the call string have a
                 single character at this position.  It must be the same
                 character or no call string satisfies them both. *)
              if i1 == i2
                then subsumes_by_parts contour_parts_1' contour_parts_2'
                else false
          | MultiPart(is1),SinglePart(i2) ->
              (* In this case, contour 1 can accept many things here but contour
                 2 can only accept one thing.  If contour 1's head option does
                 not contain contour 2's identifier, then we must hope that the
                 MultiPart on contour 1 is unnecessary here.  Otherwise, we've
                 satisfied contour 2's requirement (but we keep the MultiPart
                 around in case it's useful again). *)
              if Ident_set.mem i2 is1
                then subsumes_by_parts contour_parts_1 contour_parts_2'
                else subsumes_by_parts contour_parts_1' contour_parts_2
          | SinglePart(i1),MultiPart(is2) ->
              (* In this case, contour 2 has more options than contour 1.  Even
                 if they have some string in common, contour 2 necessarily has
                 some string which is not in contour 1. *)
              false
          | MultiPart(is1),MultiPart(is2) ->
              (* In this case, both contours have multi-parts.  If contour 1's
                 set of symbols is greater or equal, then it still has a chance;
                 otherwise, we're already finished. *)
              if Ident_set.subset is2 is1
                then subsumes_by_parts contour_parts_1 contour_parts_2'
                else false
;;

(** Determines if the first contour subsumes the second. *)
let subsumes (Contour(contour_parts_1)) (Contour(contour_parts_2)) =
  subsumes_by_parts contour_parts_1 contour_parts_2
;;

