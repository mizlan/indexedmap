open Crowbar
module IM = Indexedmap.Make (Int)

let () =
  add_test ~name:"retains elements and order" [ list int ] @@ fun l ->
  let l = List.sort Int.compare l in
  let n = List.length l in
  let s = List.fold_left IM.add IM.empty l in
  let nth_output = List.filter_map (IM.nth s) (List.init n Fun.id) in
  check_eq l (IM.to_list s);
  check_eq l nth_output
