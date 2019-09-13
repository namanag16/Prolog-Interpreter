
let rec merge ls1 ls2 = match (ls1,ls2) with
([],[]) -> []
|([],ls2) -> ls2
|(ls1,[]) -> ls1
|(x::xs,y::ys) -> if (x > y) then y :: (merge ls1 ys) else x :: (merge xs ls2);;


let rec createlist (ls,(i:int),(j:int)) = 
if i<=j then 
	(List.nth ls i) :: (createlist (ls,i+1,j))
else 
	[]
;;


let rec merge_sort ls = match ls with 
[] -> []
|[_] -> ls 
|[_;_] as temp -> if (List.nth ls 0) > (List.nth ls 1) then (List.nth ls 1) :: [(List.nth ls 0)]
				  else 
					temp
|_ -> let size = List.length(ls) in 
		let mid = size/2 in 
		merge (merge_sort (createlist (ls,0,mid))) (merge_sort (createlist (ls,mid+1,size-1)))

;;


(* 
let rec merge_sort ls =  
if List.length(ls) = 2 then begin
	if (List.nth ls 0) > (List.nth ls 1) then
		(List.nth ls 1) :: (List.nth ls 0)
	else 
		ls
end
else if List.length < 2 then 
		ls
else begin
	let size = List.length(ls) in 
	let mid = size/2 in 
	merge (my_sort (createlist ls 0 mid)) (my_sort (createlist ls mid+1 size-1))
end
;;
*)


let lst = [12;3;0;-1;47;23;10];;
merge_sort lst;;
