(*tree binar*)

type 'a bintree = Nil | Bt of 'a bintree * 'a * 'a bintree;;

(*tree*)
type 'a tree = Tree of 'a * 'a tree;;

(*tree strict binar*)
type 'a strictBT = L of 'a | SBT of 'a strictBT * 'a * 'a strictBT;;

let bt = Bt(Bt(Nil,1,Bt(Nil, 3, Nil)), 4 , Bt(Nil,6,Nil));;

let preord_bt bintree = 
  let rec preord_bt_aux bintree = 
    match bintree with 
      | Nil -> []
      | Bt(left,nod,right) -> nod :: List.append (preord_bt_aux left) (preord_bt_aux right)
  in preord_bt_aux bintree
;;

let inord_bt bintree = 
  let rec inord_bt_aux bintree= 
    match bintree with 
      | Nil -> []
      | Bt(left,nod,right) -> List.append (inord_bt_aux left) (nod :: inord_bt_aux right)
  in inord_bt_aux bintree 
;;

let postord_bt bintree = 
  let rec postord_bt_aux bintree = 
    match bintree with 
      | Nil -> []
      | Bt(left,nod,right) -> List.append (List.append (postord_bt_aux left) (postord_bt_aux right)) [nod]
  in postord_bt_aux bintree
;;

postord_bt bt;;

(*1,2,4*)