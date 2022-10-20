module StrMap = Map.Make(String);;

(*
Exercitiu 2
*)

let list_sir_to_dictionar lista =
  let rec list_sir_to_dictionar_aux lista dict nr =
    match lista with 
      | [] -> dict
      | hd :: t -> list_sir_to_dictionar_aux t (StrMap.add hd nr dict) (nr + 1)
  in list_sir_to_dictionar_aux lista StrMap.empty 1
;;

(* StrMap.bindings (list_sir_to_dictionar ["a"; "b"; "c"]);; *)

(*
Exercitiu 3
*)

let filter_w_fold dict conditie = 
  StrMap.fold (fun key element dict_conditional -> 
  if(conditie element) then StrMap.add key element dict_conditional
  else dict_conditional) dict StrMap.empty
;;

(* StrMap.bindings (filter_w_fold (list_sir_to_dictionar ["a"; "b"; "c"]) (fun x -> x>1));; *)

(*
Exercitiu 5
*)

let map_w_fold dict fnct = 
  StrMap.fold (fun key element dict_modificat -> StrMap.add key (fnct element) dict_modificat) dict StrMap.empty
;;

(* StrMap.bindings (map_w_fold (list_sir_to_dictionar ["a"; "b"; "c"]) (fun x -> x + 1));; *)

(*
Exercitiu 7
*)

let max_din_valori dict fnct = 
  StrMap.fold (fun key element maxim -> max maxim (fnct element)) dict 0;;
;;

(* max_din_valori (list_sir_to_dictionar ["a"; "b"; "c"]) (fun x -> x + 1);; *)

(*
Exercitiu 8
*)

let compunere_dict dict1 dict2 =
  StrMap.fold (fun key element dict_compus -> StrMap.add key (StrMap.find element dict1) dict_compus) dict2 StrMap.empty
;;

let list_pereche_sir lista = 
  let rec list_pereche_sir_aux lista dict =
    match lista with 
      | [] -> dict
      | (key, sir) :: t -> list_pereche_sir_aux t (StrMap.add key sir dict)
  in list_pereche_sir_aux lista StrMap.empty
;;

(* StrMap.bindings (compunere_dict (list_pereche_sir [("a", "x"); ("b", "y"); ("c", "z")]) (list_pereche_sir [("p","a"); ("q","b"); ("t", "c")]) );; *)