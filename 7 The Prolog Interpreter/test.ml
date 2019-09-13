exception Not_found;;

let summ p pt= 
	if p = 0 then raise Not_found 
	else p + pt
;; 

(* let traverse li : bool =
  let state = ref li in
  while !state <> [] do
    let x = List.hd !state in
    state := List.tl !state;
    let sum = summ x in 
    if sum = 0 then raise Not_found else false
    (* do whatever you want *)
  done
;;



  let x = try traverse ([1;2;3]) with 
 			 Not_found -> false
 			 ;;
 *)

exception Terminate_loop;;

let traver ls a : bool = 
	for i =0 to List.length(ls) - 1 do 
		let uni = summ a (List.nth ls i) in 
		if uni = 0 then (raise Terminate_loop)
		
	done 
;; 