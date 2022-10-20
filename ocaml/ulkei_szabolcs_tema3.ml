(*
Exercitiu 5
*)

let rec sumcifre n = 
  match n with 
    | 0 -> 0
    | _ -> (n mod 10) + sumcifre (n / 10)
;;

let rec nrcifre n = 
  match n with 
    | 0 -> 0
    | _ -> 1 + nrcifre (n / 10)
;;

let rec prodcifre n = 
  match n with 
    | 0 -> 1
    | _ -> (n mod 10) * prodcifre (n / 10)
;;

let rec maxcifre n = 
  match n with 
   | 0 -> 0
   | _ -> max (n mod 10) (maxcifre (n / 10))
;;

let rec mincifre n =
  match n with
    | 0 -> 10
    | _ -> min (n mod 10) (mincifre (n / 10))
;;

(*
Exercitiu 6
*)

let restmodp a p =
  let rec aux_restmodp a p k = 
    let modp = (int_of_float (a ** k)) mod  p in
      match modp = 1 with
        | true -> k
        | false when modp = 0 -> 0.
        | _ -> aux_restmodp a p (k +. 1.)
  in aux_restmodp a p 1.
;;