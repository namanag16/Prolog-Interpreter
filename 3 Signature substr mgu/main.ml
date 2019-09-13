type variable = string;;

type symbol = string;;

type term = V of variable | Node of symbol * (term list);;

type signature  = List of ((symbol * int) list);;


(****************************************************ONE********************************************************)
let rec check_dup sim = match sim with
		[] -> false
		|h::tl -> if (List.mem h tl) then true 
					else (check_dup tl);;




let rec check_sig s : bool = 
	(*are we checking any other condition for a valid signature other than these *)
	let (sim,ar) = (List.split s) in

	if ((List.exists (fun x -> x < 0) ar)) then false
	else not (check_dup sim);;




let rec wfterm s (t:term) : bool = 
	(*do we need to check the check_sig condition inside this function*)
	match t with 
	V var -> true
	|Node (x,y) ->  let arity = (try(List.assoc x s) with Not_found -> -1) in 

					if arity = List.length(y) then (List.fold_right (fun elem acc -> (wfterm s elem) && acc) y true)
					else false;;



(******************************************************THREE******************************************************)
let rec max_list l = match l with
| [] -> 0
| h :: t -> max h (max_list t)


let rec sum_list l = match l with
| [] -> 0
| h :: t -> h + sum_list t


let rec ht t = match t with 
V var -> 0
|Node (x,y) -> 1 + max_list (List.map (fun elem -> ht elem) y);;


let rec size t = match t with 
V var -> 1
|Node (x,y) -> 1 + sum_list (List.map (fun elem -> size elem) y);;

let rec vars t = match t with
V var -> [var]
|Node (x,y) ->(*  [x] @  *)(List.fold_right (fun elem acc -> (vars elem) @ acc) y []);;


(*in vars functions do we also need to include the symbols within the signature ... right now we are counting just the variables *)

(**************************************************FOUR & FIVE**********************************************************)

(*Assuming subst to be a function (term(x) -> term(x)) partial function *)
(*for composition: fog(x) = term (term(x) -> term(x)) -> (term(x) -> term(x)) -> term *)
type substitution = variable -> term;;

let rec subst (sigma:substitution) (t:term) : term = 
	match t with 
	V var -> (sigma var)
	|Node(x,y) -> if List.length(y) = 0 then Node(x,y) else Node(x,(List.map (fun elem -> (subst sigma elem)) y));;


let rec list_subst (vt_list:(variable*term) list) (t:term) : term = 
	(List.fold_right (fun elem acc -> (match elem with (var,trm) -> (subst (fun (x:variable) -> if (x = var) then trm else (V x)) acc) ) )  vt_list t);;
	
let composition (f:substitution) (g:substitution) (t:term) : term = (subst f (subst g t));;


(**************************************************SIX**********************************************************)


exception Not_Unifiable of string;;

let rec occurs (v:variable) (t:term) : bool = 
	match t with 
	V var -> var = v
	|Node(x,y) -> (List.fold_right (fun elem acc -> (occurs v elem) || acc) y false);;




let rec mgu p q = match (p,q) with 
(V x,V y) -> if x = y then [] else [(x,q)] 
|((V x,(Node(_,_) as t)) |((Node(_,_) as t), V x)) -> if (occurs x t) then raise (Not_Unifiable "due to occurs check") else [(x,t)]
|(Node(x,y),Node(p,q)) -> if (x=p && List.length(y) = List.length(q)) then unify_term_pair_list (List.combine y q)
							else raise (Not_Unifiable "due to symbol conflict")

and unify_term_pair_list (ls:(term*term) list) = 
(*Is order of execution to be handled here *)
List.fold_right (fun elem acc -> 
							(match elem with (a,b) -> 
							 let p = (list_subst acc a) 
							 and q = (list_subst acc b) in 
					 		(mgu p q) @ acc)) ls [];;  



(*************************************************SANITY TESTS***********************************************************)

check_sig ( [("+",2);("+",2);("2",0);("3",0)] );;
check_sig ([]);;

let s = [("g",2);("a",2);("2",0);("3",0)];;
wfterm s (Node("a", [V "b"; Node("g", [V "c"; V "r";V "b"])]));;


ht (Node("a", [V "b"; Node("g", [V "c"; V "r"])]));;
vars (Node("a", [V "b"; Node("g", [V "c"; V "r"])]));;
size (Node("a", [V "b"; Node("g", [V "c"; V "r"])]));;



let sub1 x = match x with
"c" ->  Node("g", [V "p"; V "r"])
|_ -> V x;;

let sub2 x = match x with 
"c" -> Node("2", [])
|_ -> V x;;

subst sub1 (Node("a", [V "b"; Node("g", [V "c"; V "r"])]));;
subst sub1 (Node("a", [V "b"; Node("g", [V "c"; V "r"])]));;
composition sub2 sub1 (Node("a", [V "b"; Node("g", [V "c"; V "r"])]));;



mgu (Node ("a", [V "b"; Node ("g", [V "c"; V "r"])])) (Node ("a", [Node ("g", [V "c"; V "r"]); Node ("g", [V "c"; V "r"])]));;
mgu (Node ("x", [ Node("c",[]); Node("d",[]) ])) (Node ("g", [ Node("c",[]); Node("d",[]) ]));;
mgu (V "v") (V "v");;


