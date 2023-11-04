open Crowbar
module IM = Indexedmap.Make (Int)
module IS = Indexedmap.Make (String)

(*
 * tests on sets, i.e., maps whose values are ()
 *)

let () =
  add_test ~name:"retains elements and order" [ list int ] @@ fun l ->
  let ul = List.sort_uniq Int.compare l in
  let n = List.length ul in
  let us = List.fold_right (fun x -> IM.add x ()) ul IM.empty in
  let s = List.fold_right (fun x -> IM.add x ()) l IM.empty in
  let nth_output =
    List.filter_map (fun i -> IM.nth i us) (List.init n Fun.id)
  in
  let nth_output_undeduped =
    List.filter_map (fun i -> IM.nth i s) (List.init n Fun.id)
  in
  check_eq ul (List.map fst (IM.to_list us));
  check_eq ul (List.map fst nth_output);
  check_eq ul (List.map fst nth_output_undeduped)

let () =
  add_test ~name:"retains elements and order for strings" [ list bytes ]
  @@ fun l ->
  let ul = List.sort_uniq String.compare l in
  let n = List.length ul in
  let us = List.fold_right (fun s -> IS.add s ()) ul IS.empty in
  let s = List.fold_right (fun x -> IS.add x ()) l IS.empty in
  let nth_output =
    List.filter_map (fun i -> IS.nth i us) (List.init n Fun.id)
  in
  let nth_output_undeduped =
    List.filter_map (fun i -> IS.nth i s) (List.init n Fun.id)
  in
  check_eq ul (List.map fst (IS.to_list us));
  check_eq ul (List.map fst nth_output);
  check_eq ul (List.map fst nth_output_undeduped)

let () =
  add_test ~name:"can rank integers" [ list int ] @@ fun l ->
  let ul = List.sort_uniq Int.compare l in
  let n = List.length ul in
  let us = List.fold_right (fun x -> IM.add x ()) ul IM.empty in
  (* test building from un-deduped list to check that IM removes duplicates *)
  let s = List.fold_right (fun x -> IM.add x ()) l IM.empty in
  let ranks_eq =
    List.for_all2 (fun v i -> IM.rank v us = i) ul (List.init n Fun.id)
    && List.for_all2 (fun v i -> IM.rank v s = i) ul (List.init n Fun.id)
  in
  check ranks_eq

let () =
  add_test ~name:"can rank strings" [ list bytes ] @@ fun l ->
  let ul = List.sort_uniq String.compare l in
  let n = List.length ul in
  let us = List.fold_right (fun x -> IS.add x ()) ul IS.empty in
  (* test building from un-deduped list to check that IM removes duplicates *)
  let s = List.fold_right (fun x -> IS.add x ()) l IS.empty in
  let ranks_eq =
    List.for_all2 (fun v i -> IS.rank v us = i) ul (List.init n Fun.id)
    && List.for_all2 (fun v i -> IS.rank v s = i) ul (List.init n Fun.id)
  in
  check ranks_eq
