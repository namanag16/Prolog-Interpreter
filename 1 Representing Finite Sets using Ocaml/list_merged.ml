let emptyset = [] ;;

let isempty ls = match ls with 
[] -> true
|_ -> false;;

let rec member x s = match s with 
[] -> false
|h::tl -> if h == x then true else (member x tl);;

let rec subset s1 s2 = 
	let s11 = List.sort compare s1 in 
	let s21 = List.sort compare s2 in 
	match s11 with 
	[] -> true
	|h::tl -> match s21 with 
				[] -> false
				|x :: xs -> if h > x then (subset s1 xs) 
							else if h == x then (subset tl xs)
							else false ;;


let set1 = [2;-1;3];;
let set2 = [1;2;3;4;-1];;

print_string("check for empty set ");;
isempty emptyset ;; 
isempty [[]];;
isempty [2;3;4];;

print_string("check member");;
member 0 set1;;
member (-1) set1;;

print_string("check subset");;
subset set1 set2;;
subset [1;2;3;4;-1;5] set2;;
subset [] set1;;
subset [2;3] [];;



(**********************************************************************************************************************************)



let rec rem_dup ls = match ls with 
[] -> []
|x::xs -> match xs with 
			[] -> [x]
			|h::tl -> if h == x then h :: rem_dup tl 
						else x :: rem_dup xs
;;


let rec union ls1 ls2 = 
	rem_dup (List.sort compare (ls1 @ ls2));;

let rec equal set1 set2 = 
	let ls1 = List.sort compare set1 in 
	let ls2 = List.sort compare set2 in 
	match ls1 with 
	[] -> if ls2 == [] then true else false
	|h::tl -> match set2 with 
				[] -> false
				|x :: xs -> if h == x then (equal tl xs) else false;;


let rec intersection set1 set2 = 
	let ls1 = List.sort compare set1 in 
	let ls2 = List.sort compare set2 in 
	match ls1 with 
	[] -> []
	|h::tl -> match ls2 with 
				[] -> []
				|x :: xs -> if h > x then (intersection ls1 xs) 
							else if h == x then h :: (intersection tl xs)
							else (intersection tl ls2);;



let ls1 = [2;3;4];;
let ls2 = [2;3;4;5;6];;

print_string("check union");;
union ls1 ls2;;
union [] ls2;;
union ls1 [];;

print_string("check equal");;
equal ls1 [2;3;4];;
equal [] [];;


print_string("check intersection");;
intersection [1;6] [6];;
intersection [] [1;5];;
intersection [1;4] [];;
intersection [1;5] [7;8];;
intersection [6] [1;6];;



(**********************************************************************************************************************************)






let rec difference set1 set2 = 
	let ls1 = List.sort compare set1 in 
	let ls2 = List.sort compare set2 in 
	match ls1 with  
	[] -> []
	|h::tl -> match ls2 with 
				[] -> set1
				|x :: xs -> if h > x then (difference ls1 xs) 
							else if h == x then (difference tl xs)
							else h :: (difference tl ls2);;



let rec powerset ls = match ls with 
[] -> [[]]
| h::tl -> List.fold_left (fun acc x -> (h :: x):: x :: acc) [] (powerset tl) ;;
	



let product (set1:'a list) (set2:'a list ) : 'a list list  = 
	if set1 == [] || set2 == [] then 
		[[]]
	else List.fold_right (fun x acc -> (List.fold_left (fun acc1 x1 -> (x :: [x1]) :: acc1) [] set2) @ acc) set1 [];;



print_string("check difference");;
difference [2;3] [-1] ;;
difference [-1] [2;3];;
difference [] [];;
difference [] [2];;
difference [2;3] [];;
difference [2;3] [2;3];;


print_string("check power");;
powerset [];;
powerset [1];;
powerset [2;3];;

print_string("check product");;
product [] [];;
product [] [2;3];;
product [2;3] [] ;;
product [2] [2;4];;




(**********************************************************************************************************************************)
print_string("start real test cases");;
let s = [];;
let s1 = [1;2;3];;
let s2 = [1;2;3;4;5];;
let s3 = [1;2;7;8;9];;



isempty(s);;
isempty(s1);;

member 3 s2;;

subset s1 s2;;

equal s2 s1;;
equal s2 s2;;
equal s s1;;

union s1 s2;;
union s s1;; 

intersection s1 s2;;
intersection s1 s3;;

difference s1 s2;;
difference s1 s3;;

powerset s1;;

product s1 s2;;
