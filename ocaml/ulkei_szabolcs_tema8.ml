module StrMap = Map.Make(String);;

(*
Exercitiu 4
*)

let  dict1 = 
  StrMap.empty
  |> StrMap.add "a" 2
  |> StrMap.add "b" 1
  |> StrMap.add "c" 3
;;

let exists key valoare dict = 
  try StrMap.fold (fun k v bool -> if(k = key & valoare = v) then failwith "found" else false) dict false
  with found -> true
;;

let for_all conditie dict =
  try StrMap.fold (fun k v bool -> if(conditie k v) then true else failwith "found_exception")dict true
  with found_exception -> false
;;

(*
Exercitiu 10
*)

let dict2 = 
  StrMap.empty
  |> StrMap.add "a" "c"
  |> StrMap.add "b" "a"
  |> StrMap.add "c" "d"
;;

let compunere dict1 dict2 = 
  StrMap.fold (fun key valoare dict_rez -> 
    if(StrMap.find valoare dict2 = valoare) then failwith "punct fix"
    else StrMap.add key (StrMap.find valoare dict2) dict_rez ) dict1 StrMap.empty
;;

let f_2 dict = 
  compunere dict dict
;;

let f_n dict n =
  let rec f_n_aux dict n dict_rez=
    match n with
      | 1 -> dict_rez
      | _ when dict_rez = StrMap.empty -> f_n_aux dict (n-1) (f_2 dict)
      | _ -> f_n_aux dict (n-1) (compunere dict dict_rez)
  in 
  try f_n_aux dict n StrMap.empty
  with Not_found -> StrMap.empty;;
;;

StrMap.bindings (f_n dict2 8);;