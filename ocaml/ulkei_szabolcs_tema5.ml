(*
Exercitiu 2
*)

let from_to_divizibil fr t d =
  let rec aux_from_to_divizibil fr t d l =
    match t > fr with
      | true when t mod d = 0 -> aux_from_to_divizibil fr (t - d) d (t :: l)
      | true -> aux_from_to_divizibil fr (t - 1) d l
      | false -> l
  in aux_from_to_divizibil fr t d []
;;

(*
Exercitiu 7
*)

let combine lper = 
  let rec aux_combine l1 l2 l =
    match l1 with 
      | [] -> l
      | h :: t -> aux_combine t (List.tl l2) ((h, List.hd l2) :: l)
  in 
  match lper with
  | (l1, l2) -> aux_combine l1 l2 []
  | _ -> [(0, 0)]
;;

let split l = 
  let rec aux_split l l1 l2 = 
    match l with
      | [] -> (l1,l2)
      | (h1, h2) :: t -> aux_split t (h1 :: l1) (h2 :: l2)
    in aux_split l [] []
;;
(*
combine ([1;2;3;4;5;6], [1;2;3;4;5;6]);;
split [(1, 2); (3, 4); (5, 6)];;
*)

(*
Exercitiu 11
*)

let comp_list l1 l2 = 
  let delta_length = List.length l1 - List.length l2 in
  if(delta_length > 0) then -1
  else if (delta_length < 0) then 1
  else (
    let rec aux_comp_list l1 l2 = 
      if l1 = [] then 0
      else match List.hd l1 = List.hd l2 with
        | false when List.hd l1 > List.hd l2 -> -1
        | false when List.hd l1 < List.hd l2 -> 1
        | _ -> aux_comp_list (List.tl l1) (List.tl l2)
    in aux_comp_list l1 l2
  )
;;


(*
Exercitiu 12
*)

let merge_ord l =
  let rec aux_merge_ord l lf =
    match l with 
      | (h1 :: t1, h2 :: t2) when h1 < h2 -> aux_merge_ord (t1, h2 :: t2) (h1 :: lf)
      | (h1 :: t1, h2 :: t2) when h1 >= h2 -> aux_merge_ord (h1 :: t1, t2) (h2 :: lf)
      | ([], h2 :: t2) -> aux_merge_ord ([], t2) (h2 :: lf)
      | (h1 :: t1, []) -> aux_merge_ord (t1, []) (h1 :: lf)
      | _ -> lf
  in aux_merge_ord l []
;;

merge_ord ([1;3;5;7;9], [0;2;4;6;8]);;