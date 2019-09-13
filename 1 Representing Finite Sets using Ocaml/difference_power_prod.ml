
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
