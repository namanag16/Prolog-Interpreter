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