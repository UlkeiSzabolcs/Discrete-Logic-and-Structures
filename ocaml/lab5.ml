let l = [1;2;3;4];;

(*List.fold_left (fun a b -> b :: a) [] l;;
List.fold_left (fun a b -> b + 5 :: a) [] l;;
List.fold_left (fun a b -> if b > 2 then b :: a else a) [] l;;
List.fold_right (fun a b -> a :: b) l [];;*)


(*
Exercitiu 6
*)
let cond x = x > 2;;

let count_if cond l = 
  List.fold_left (fun nr e -> if cond e then nr + 1 else nr ) 0 l
;;

let sum_if cond l = 
  List.fold_left (fun nr e -> if cond e then nr + e else nr) 0 l
;;

(*
Exercitiu 8
*)

let list_partition cond l =
  (List.fold_left (fun ln e -> if cond e then e :: ln else ln) [] l ,
   List.fold_left (fun ln e -> if not (cond e) then e :: ln else ln) [] l)
;;

(*
Exercitiu 11
*)

let comp_list l1 l2 = 
  let delta_length = List.length l1 - List.length l2 in
  if(delta_length > 0) then -1
  else if (delta_length < 0) then 1
  else (
    (*List.fold_left (fun value (l1,l2) -> 
    if (List.hd l1 > List.hd l2) && (value = 0) then 1 
    else if (List.hd l1 < List.hd l2) && (value = 0) then -1 
    else 0)
    0 [(l1,l2)]*)
    if(List.fold_left (fun value (l1,l2) -> 
    if (List.hd l1 > List.hd l2) && (value = 0) then 1 
    else if (List.hd l1 < List.hd l2) && (value = 0) then -1 
    else 0)
    0 l1
  )
;;

comp_list [1;2;3] [1;2;4];;
