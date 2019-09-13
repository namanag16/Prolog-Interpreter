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

