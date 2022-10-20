(*
Exercitiu 2 
*)

let grd2 a b c =
   let delta = b * b - 4 * a * c in
    match delta > 0 with
    | false  when delta = 0 -> print_float (-. (float b) /. (2. *. (float a)))
    | true -> Printf.printf "%f, %f" ((-. (float b) -. (sqrt (float delta))) /. (2. *. (float a))) ((-. (float b) +. (sqrt (float delta))) /. (2. *. (float a)))
    | _ -> print_string "nu exista solutii reale"
;;

grd2 1 0 (-4) ;;

(*
Exercitiu 5
*)

let mediana a b c = max (min b c) (max (min a b) (min a c));;

(*
Exercitiu 6
*)

let add f g x = f x + g x;;
let prod f g x = f x * g x ;;
let f x = x + 3;;
let g x = x + 4;;
