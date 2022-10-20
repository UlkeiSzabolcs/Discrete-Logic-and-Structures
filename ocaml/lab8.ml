(*6 9 2 5 8*)

module StrMap = Map.Make(String);;

module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make(Int);;

(*
Exercitiu 6 dict
*)

let corespunde dict lista = 
  StrMap.fold
  (fun key element multime -> 
  if(List.exists (fun x -> x = key) lista) 
  then IntSet.add element multime
  else multime) dict IntSet.empty
;;

(*
Exercitiu 9 dict
*)

let depth sir dict = 
  let rec depth_aux sir dict n = 
    let k = try StrMap.find sir dict with
      | Not_found -> "-1"
    in
    match n > StrMap.cardinal dict with
    | true -> -1
    | false when k = "-1" -> n
    | _ -> depth_aux k dict (n + 1)
  in depth_aux sir dict 0
;;

(*
Exercitiu 2 rel
*)
