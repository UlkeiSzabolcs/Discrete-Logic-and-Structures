let rec listacifre_inv n = 
  match n with
   | 0 -> []
   | _ -> (n mod 10) :: listacifre_inv (n / 10)
;;

let rec listacifre n l = 
  match n with
   | 0 -> l
   | _ -> listacifre (n / 10) ((n mod 10) :: l)
;;

let rec sumlist l = 
  match l with 
    | [] -> 0
    | h :: t -> h + sumlist t
;;

let rec cautmem l x = 
  match l with
    | [] -> false
    | h :: t -> h = x || cautmem t x
;;

let rec eq12 l = 
  match l with 
    | e1 :: e2 :: _ -> e1 = e2
    | _ -> false
;;

let  listcond n f =
  let rec listcond_aux n f l =
    match n with
      | 0 -> l
      | _ -> if f (n mod 10) then listcond_aux (n / 10) f ((n mod 10) :: l) else l
  in listcond_aux n f []
;;

let f x = x > 3;;

let list_to_nr l = 
  let rec list_to_nr_aux l n =
    match l with 
      | [] -> n
      | h :: t -> list_to_nr_aux t (n * 10 + h)
  in list_to_nr_aux l 0
;;

list_to_nr [1;2;3;4;5];;