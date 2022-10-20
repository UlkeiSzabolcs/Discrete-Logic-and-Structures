(* exercitiu 1 *)

let bisect an = 
  if (an mod 4) != 0 then
    print_string "nu"
  else if (an mod 100) == 0 && (an mod 400) != 0 then
    print_string "nu"
  else 
    print_string "da"
;;

bisect 500;;

(* exercitiu 2 *)

let doivalori a b c = 
  if a == b then
    print_string  "argumentele 1 și 2 sunt egale" 
  else if a == c then 
  print_string  "argumentele 1 și 3 sunt egale" 
  else 
  print_string  "argumentele 2 și 3 sunt egale" 
;;


let distincte a b c =
  if a != b && b !=c && a !=c then
    print_string "trei valori distincte"
  else if a == b && a == c then
    print_string "toate argumentele sunt egale"
  else 
    doivalori a b c
;;

distincte 1 2 3;;