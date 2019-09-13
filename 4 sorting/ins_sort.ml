(*This code doesnt work... fucntion ins_sort has a problem*)

let rec insert elem ls = match ls with 
[] -> [elem]
|x::xs -> if x > elem then elem :: x :: xs
			else x :: insert elem xs;;


let rec ins_sort ls = match ls with 
[] -> []
|h::tl -> match tl with 
			[] -> [h]
			|x::xs -> (insert x [h]) @ (ins_sort xs)
;;


let ls = [12;3;9;0;5];;
ins_sort ls;;
