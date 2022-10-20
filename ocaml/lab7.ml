(*Dictionari*)

module StrMap = Map.Make(String);;
(*cheii sunt de tipul String*)

let dict = StrMap.add "a" 1 StrMap.empty;;
let dict2 = StrMap.empty |> StrMap.add "a" 1 ;;
let dict3 = StrMap.empty |> StrMap.add "a" 1 |> StrMap.add "b" 2 ;;

StrMap.bindings (dict3);;

let find chr =
  try StrMap.find chr dict3 with
    | Not_found -> 0
;;

let contine chr str = String.contains chr str ;;

let map_cont dict =
  StrMap.fold (fun key vlr dict_final -> 
    if(contine key 'a') 
    then StrMap.add key (vlr * vlr) dict_final 
    else dict_final) dict StrMap.empty
;;

let dict4 = StrMap.empty |> StrMap.add "ab" 1 |> StrMap.add "aa" 2 
|> StrMap.add "cc" 0 |> StrMap.add "da" 11;;

StrMap.bindings (map_cont dict4) ;;

(*
Exercitiu 1
*)

let suma_intregilor lista sir = 
  let rec suma_intregilor_aux lista sum sir = 
    match lista with 
      | [] -> sum
      | (e1 , e2) :: t when e1 = sir -> suma_intregilor_aux t (sum + e2) sir
      | (e1 , e2) :: t -> suma_intregilor_aux t sum sir
  in suma_intregilor_aux lista 0 sir
;;

let asocia dict (sir, intreg) lista = StrMap.add sir (suma_intregilor lista sir) dict;;



let autentification dict (sir, intreg) lista= 
  let verification_aux dict (sir, intreg) = 
    try StrMap.find sir dict 
    with Not_found -> -1
    in
  if (verification_aux dict (sir, intreg) = (-1)) then asocia dict (sir, intreg) lista
  else dict 
;;

let list_to_map lista = 
  List.fold_left (fun dict (sir, intreg) -> autentification dict (sir, intreg) lista) StrMap.empty lista
;;

StrMap.bindings (list_to_map [("a", 5); ("b", 1); ("c", 3); ("a", 5)]);;