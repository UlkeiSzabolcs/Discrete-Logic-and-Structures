let rec suma n = 
  match n = 0 with 
    | true -> 0
    | false -> n + suma (n-1) 
;;

(*
let rec hanoi n from_index help_index to_index = 
  if n = 1 then(
    Printf.printf "%d -> %d /n" from_index to_index;
  )
  else( hanoi (n-1) from_index to_index help_index)
  hanoi (n-1) from_index to_index help_index
;;
*)

(*
Exercitiu 1
*)

let rec factorial n = 
  if n = 1 then 1
  else n * (factorial (n-1))
;;

let rec cmmdc a b =
  let r = a mod b in 
  if r = 0 then b
  else cmmdc b r
  ;;

let rec compunere n f x= 
  if n = 1 then f x
  else f (compunere (n-1) f x)
;;

let f x = x + 5;;

compunere 3 f 5;;