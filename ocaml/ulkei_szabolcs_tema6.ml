module SetChar = Set.Make(Char);;

module Int = struct
  type t = int
  let compare = compare
end

module SetInt = Set.Make(Int);;

let rec list_to_multime_char list = 
  match list with
    | [] -> SetChar.empty;
    | h :: t -> SetChar.add h (list_to_multime_char t)
;;

let rec list_to_multime_int list = 
  match list with
    | [] -> SetInt.empty;
    | h :: t -> SetInt.add h (list_to_multime_int t)
;;

(*
Exercitiu 1
*)

let afise multime = 
  SetChar.iter (fun element -> Printf.printf "%c, " element) multime
;;

afise (list_to_multime_char ['a'; 'b'; 'c']);;

(*
Exercitiu 4
*)

let conditie x = x > 5;;

let partition conditie multime = 
  SetInt.fold (fun element (sat, nesat) -> if(conditie element) then ((SetInt.add element sat), nesat) 
    else (sat, SetInt.add element nesat)) multime (SetInt.empty, SetInt.empty)
;;

partition conditie (list_to_multime_int [1;2;3;4;5;6]);;

(*
Exercitiu 6
*)

let numar_to_cifre n = 
  let rec numar_to_cifre_aux n m =
    match n with 
      | 0 -> m
      | _ -> numar_to_cifre_aux (n/10) (SetInt.add (n mod 10) m)
  in numar_to_cifre_aux n SetInt.empty
;;

let numere_to_cifre multime = 
  SetInt.fold (fun element m -> SetInt.union (numar_to_cifre element) m) multime SetInt.empty
;;

SetInt.elements (numere_to_cifre (list_to_multime_int [123; 456; 784]));;

(*
Execitiu 9
*)

let f a b = a + b;;

let pereche f element multime multime_partial= 
  SetInt.fold (fun element2 multime_partial -> SetInt.add (f element element2) multime_partial) multime multime_partial
;;

let ex9 f multime1 multime2 = 
   SetInt.fold (fun element multime_final -> SetInt.union (pereche f element multime2 SetInt.empty) multime_final) multime1 SetInt.empty
;;

SetInt.elements (ex9 f (list_to_multime_int [1;2;3;4;5]) (list_to_multime_int [9;8;7;6;5]) );;