(*
module Stringset = Set.Make(String);;
module Int = struct 
  type t = int
  let compare = compare (*Pervasives.compare*)
end

module IntSet = Set.Make(Int);;

IntSet.add 1 IntSet.empty;;
IntSet.add 1 (IntSet.singleton 2);
*)

(*
module Int = struct 
  type t = int
  let compare = compare (*Pervasives.compare*)
end

module IntSet = Set.Make(Int);;

let rec set_of_intlist l m =
  match l with
    | [] -> m
    | h :: t -> set_of_intlist t (IntSet.add h m)
;;

let trans l = 
  List.fold_left (fun m e -> IntSet.add e m) IntSet.empty l;;

let rezult = trans [1;2;3;4];;

IntSet.elements rezult;;
*)

module Int = struct 
  type t = int
  let compare = compare (*Pervasives.compare*)
end

module IntSet = Set.Make(Int);;

module CharSet = Set.Make(Char);;

module IntChar = struct
  type t = int * char
  let compare = compare
end

module IntCharSet = Set.Make(IntChar);;

let pereche x mC mPA= 
  CharSet.fold (fun mc mic -> IntCharSet.add (x,mc) mic ) mC IntCharSet.empty
;;

let produs_cartezian mI mC = 
  IntSet.fold (fun mi mPa -> pereche mi mC mPa) mI IntCharSet.empty
;;
