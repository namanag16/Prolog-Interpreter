(*********************************************************************************)

let set1 (x:int) = 
	let ls = [2;3;4;7] in 
	try (x == List.find (fun a -> (a == x)) ls) with
		Not_found -> false;;

let set2 (x:int) = 
	let ls = [3;12;1;5] in 
	try (x == List.find (fun a -> (a == x)) ls) with
		Not_found -> false;;

(*********************************************************************************)
type 'a func_set = 'a -> bool;;

let (emptyset:'a func_set) = fun x -> false;;

let member (x:'a) (set:'a func_set) : bool = set x;;

let union (set1: 'a func_set) (set2:'a func_set) : 'a func_set = 
	(fun x -> ((set1 x) || (set2 x)))
;;

let intersection (set1:'a func_set) (set2:'a func_set) : 'a func_set = 
	(fun x -> (set1 x) && (set2 x))
;;

let difference (set1:'a func_set) (set2:'a func_set) : 'a func_set = 
	(fun x -> (set1 x) && (Pervasives.not (set2 x)))
;;

let product (set1:'a func_set) (set2:'a func_set) : (int*int) func_set = 
	(fun (x,y) -> (set1 x) && (set2 y))
;;

(*********************************************************************************)

let power (set1:'a func_set) : ('a func_set func_set) = 
	(fun set -> (subset set set1))
;;


let subset (set1:'a func_set) (set2:'a func_set) : bool = 
	true;;
;;


let equal (set1:'a func_set) (set2:'a func_set) : bool = 
	false
;;


(*********************************************************************************)
print_string("check membership");;
member 3 set1;;
member 9 set2;;
member 1 emptyset;;


print_string("check subset");;
subset set1 set2;;


member 7 (difference set1 set2);;
member 7 (union set1 set2);;
member 7 (intersection set1 set2);;

member (7,56) (product set1 set2);;



(***************************************************************************************)
print_string("start real test cases ");;

let f_s1 x= if (x mod 3=0) then true else false;;
let f_s2 x= if (x mod 5=0) then true else false;;
let f_s3 x = if (x mod 15=0) then true else false;;

(** issubset takes range from 1-100 **)

subset f_s1 f_s2 100;;

member 5 f_s1;;
member 15 f_s2;;

union f_s1 f_s2 14;;
union f_s1 f_s2 30;;
(*
equal f_s1 f_s1 100;;
equal f_s1 f_s2 100;;

(** isempty takes range from 1-100 **)

isempty f_s1 100;;
isempty f_s1 2;;
*)
intersection f_s1 f_s2 15;;
intersection f_s1 f_s2 5;;

difference f_s1 f_s2 9;;
difference f_s1 f_s2 15;;

