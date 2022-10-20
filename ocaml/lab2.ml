(*boolean

let bisect an = (an mod 4 = 0 && (an mod 100 <> 0 || an mod 400 = 0));;
*)

(*match

let f a = match a with
  | 5 | 20 | 26 when  a mod 5 = 0 -> a/5
  | 26 | 27 | 36 when a mod 6 = 0 -> a/6
  | _ -> a 
;;
*)

(* compunere

let f x = x / 2;;

let g x = x + 3;;

let comp f g x = f(g x);;

comp f g 5;;
*)

(*

let distincte a b c = match a=b with
  | true when b = c -> print_string "cei 3 sunt egal"
  | true when b <> c -> print_string "argumentele 1 și 2 sunt egale"
  | false when b = c -> print_string "argumentele 2 și 3 sunt egale"
  | false when a = c -> print_string "argumentele 1 și 3 sunt egale"
  | _ -> print_string "valorii diferite"
;;
*)

(*

let mediana a b c = max (min b c) (max (min a b) (min a c));;
mediana 3 1 2;;
*)

(*

let add f g x = f x + g x;;
let prod f g x = f x * g x ;;
let f x = x + 3;;
let g x = x + 4;;

add f g 5;;
prod f g 5;;
*)