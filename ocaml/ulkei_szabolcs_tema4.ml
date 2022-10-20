(*
Exercitiu 4
*)

let n_rand n b =
  let rec n_rand_aux n b l =
    match n with
      | 0 -> l
      | _ -> n_rand_aux (n - 1) b ((Random.int b) :: l)
  in n_rand_aux n b []
;;

(*
Exercitiu 9
*)

let list_to_nr l = 
  let rec list_to_nr_aux l n =
    match l with 
      | [] -> n
      | h :: t -> list_to_nr_aux t (n * 10 + h)
  in list_to_nr_aux l 0
;;

(*
Exercitiu 10
*)

let rec elim_dupl l =
  match l with
    | [] -> []
    | e1 :: e2 :: t when e1 = e2 -> e1 :: elim_dupl t
    | h :: t -> h :: elim_dupl t
;;

elim_dupl [1;1;2;2;3;3];;