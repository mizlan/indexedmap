module IM = Indexedmap.Make (Int)
module SM = Indexedmap.Make (String)

(*
 * tests on sets, i.e., maps whose values are ()
 *)

let t0 =
  QCheck2.(
    Test.make ~name:"int: retains elements" ~count:1_000
      ~print:Print.(list int)
      Gen.(list int)
    @@ fun l ->
    let s = List.fold_right (fun x -> IM.add x ()) l IM.empty in
    List.for_all (fun i -> IM.mem i s) l)

let t00 =
  QCheck2.(
    Test.make ~name:"int: removes dups" ~count:1_000
      ~print:Print.(list int)
      Gen.(list int)
    @@ fun l ->
    let s = List.fold_right (fun x -> IM.add x ()) l IM.empty in
    let deduped_l = List.sort_uniq Int.compare l in
    deduped_l = List.map fst (IM.to_list s))

let t1 =
  QCheck2.(
    Test.make ~name:"int: retains order" ~count:1_000
      ~print:Print.(list int)
      Gen.(list_size (0 -- 20) int)
    @@ fun l ->
    let ul = List.sort_uniq Int.compare l in
    let n = List.length ul in
    let us = List.fold_right (fun x -> IM.add x ()) ul IM.empty in
    let nth_output =
      List.filter_map (fun i -> IM.nth i us) (List.init n Fun.id)
    in
    ul = List.map fst (IM.to_list us) && ul = List.map fst nth_output)

let t2 =
  QCheck2.(
    Test.make ~name:"string: retains order"
      ~print:Print.(list string)
      Gen.(list_size (0 -- 20) string)
    @@ fun l ->
    let ul = List.sort_uniq String.compare l in
    let n = List.length ul in
    let us = List.fold_right (fun s -> SM.add s ()) ul SM.empty in
    let nth_output =
      List.filter_map (fun i -> SM.nth i us) (List.init n Fun.id)
    in
    ul = List.map fst (SM.to_list us) && ul = List.map fst nth_output)

let t20 =
  QCheck2.(
    Test.make ~name:"string: retains order with dups"
      ~print:Print.(list string)
      Gen.(list_size (0 -- 20) string)
    @@ fun l ->
    let ul = List.sort_uniq String.compare l in
    let n = List.length ul in
    let s = List.fold_right (fun x -> SM.add x ()) l SM.empty in
    let nth_output_undeduped =
      List.map fst @@ List.filter_map (fun i -> SM.nth i s) (List.init n Fun.id)
    in
    SM.length s = List.length ul
    && ul = List.map fst (SM.to_list s)
    && ul = nth_output_undeduped)

let t3 =
  QCheck2.(
    Test.make ~name:"can rank integers"
      ~print:Print.(list int)
      Gen.(list_size (0 -- 20) int)
    @@ fun l ->
    let ul = List.sort_uniq Int.compare l in
    let n = List.length ul in
    let us = List.fold_right (fun x -> IM.add x ()) ul IM.empty in
    (* test building from un-deduped list to check that IM removes duplicates *)
    let s = List.fold_right (fun x -> IM.add x ()) l IM.empty in
    let ranks_eq =
      List.for_all2 (fun v i -> IM.rank v us = i) ul (List.init n Fun.id)
      && List.for_all2 (fun v i -> IM.rank v s = i) ul (List.init n Fun.id)
    in
    ranks_eq)

(* let () = *)
(*   add_test ~name:"can rank strings" [ list bytes ] @@ fun l -> *)
(*   let ul = List.sort_uniq String.compare l in *)
(*   let n = List.length ul in *)
(*   let us = List.fold_right (fun x -> SM.add x ()) ul SM.empty in *)
(*   (* test building from un-deduped list to check that IM removes duplicates *) *)
(*   let s = List.fold_right (fun x -> SM.add x ()) l SM.empty in *)
(*   let ranks_eq = *)
(*     List.for_all2 (fun v i -> SM.rank v us = i) ul (List.init n Fun.id) *)
(*     && List.for_all2 (fun v i -> SM.rank v s = i) ul (List.init n Fun.id) *)
(*   in *)
(*   check ranks_eq *)
(**)
(* (* *)
   (*     * tests on maps to int *)
   (*     *) *)
(**)
(* let () = *)
(*   add_test ~name:"can act as frequency map" [ list (tuple (int, int)) ] *)
(*   @@ fun l -> *)
(*   let ul = List.sort_uniq Int.compare l in *)
(*   let n = List.length ul in *)
(*   let us = List.fold_right (fun x -> IM.add x ()) ul IM.empty in *)
(*   let s = List.fold_right (fun x -> IM.add x ()) l IM.empty in *)
(*   let nth_output = *)
(*     List.filter_map (fun i -> IM.nth i us) (List.init n Fun.id) *)
(*   in *)
(*   let nth_output_undeduped = *)
(*     List.filter_map (fun i -> IM.nth i s) (List.init n Fun.id) *)
(*   in *)
(*   check_eq ul (List.map fst (IM.to_list us)); *)
(*   check_eq ul (List.map fst nth_output); *)
(*   check_eq ul (List.map fst nth_output_undeduped) *)

let () =
  let set_suite =
    List.map QCheck_alcotest.to_alcotest [ t0; t00; t1; t2; t20; t3 ]
  in
  Alcotest.run "my test" [ ("set", set_suite) ]
