(************************************TYPE DECLARATION********************************************)
type prop = Atom of string
|T |F
|And of prop * prop
|Not of prop
|Or of prop * prop
|Imply of prop * prop
|Iff of prop * prop;;

let a0 = Atom "pepo";;
let a1 = Atom "A";;
let a2 = Not (Atom "B");;
let a3 = Not a1;;
let a4 = And (a2,a1);;
let a5 = Iff (a0,a4);;
let a6 = Not a5;;

(*****************************************NNF CONVERSION**************************************************)

let rec nnf (p:prop) = match p with 
	Atom s -> p
	|T -> p
	|F -> p
	|And(p1,p2) -> And((nnf p1),(nnf p2))
	|Or(p1,p2) -> Or((nnf p1),(nnf p2))
	|Imply(p1,p2) -> Or(nnf(Not p1),nnf p2)
	|Iff(p1,p2) -> Or(And(nnf p1,nnf p2),And(nnf(Not p1),nnf(Not p2)))
	|Not p1 -> match p1 with 
				Atom s -> p
				|T -> F
				|F -> T
				|And(p1,p2) -> Or(nnf(Not p1),nnf(Not p2))
				|Or(p1,p2) -> And(nnf(Not p1),nnf(Not p2))
				|Imply(p1,p2) -> And(nnf p1,nnf (Not p2))
				|Iff(p1,p2) -> And(Or(nnf p1,nnf p2),Or(nnf(Not p1),nnf(Not p2)))
				|Not p1 -> nnf p1;;


nnf a6;;

(*****************************************HELPER FUNCTIONS**************************************************)

module StringSet  = Set.Make(String);;
module ClauseSet = Set.Make(StringSet);;

(* let print x = 
	Printf.printf "I'm looking at element %d now\n" x
	print_endline y;; *)

let print_set s = 
	 StringSet.iter print_endline s ;;

let print_clause c =  
	ClauseSet.iter print_set c;;	

let rec getLiterals p = 
	let retset = StringSet.empty in 

	match p with
	Atom s -> StringSet.add s retset
	|T -> StringSet.add "T" retset
	|F -> StringSet.add "F" retset
	|And(p1,p2) -> StringSet.union (getLiterals p1) (getLiterals p2)
	|Or(p1,p2) -> StringSet.union (getLiterals p1) (getLiterals p2)
	|Imply(p1,p2) -> StringSet.union (getLiterals p1) (getLiterals p2)
	|Iff(p1,p2) -> StringSet.union (getLiterals p1) (getLiterals p2)
	|Not p1 -> match p1 with 
				Atom s -> StringSet.add (String.concat " " ["Not";s]) retset;;


let rec to_cnf (p:prop)= 

	match p with 
	Atom s -> p 
	|T -> p
	|F -> p
	|Not p1 -> p
	|And(p1,p2) -> And(to_cnf p1,to_cnf p2)
	|Or(p1,p2) -> match (p1,p2) with 
					(And(f1, f2), And(f3, f4)) -> And(And(Or(f1, f3), Or(f1, f4)), And(Or(f2, f3), Or(f2, f4)))
					|(f1, And(f2, f3)) -> And(Or(f1, f2), Or(f1, f3))
					|(And(f2, f3), f1) -> And(Or(f1, f2), Or(f1, f3))
					|_ -> p;;


to_cnf (nnf a6);;


let rec to_dnf (p:prop)= 

	match p with 
	Atom s -> p 
	|T -> p
	|F -> p
	|Not p1 -> p
	|Or(p1,p2) -> Or(to_dnf p1,to_dnf p2)
	|And(p1,p2) -> match (p1,p2) with
					(Or(f1, f2), Or(f3, f4)) -> Or(Or(And(f1, f3), And(f1, f4)), Or(And(f2, f3), And(f2, f4)))
					|(f1, Or(f2, f3)) -> Or(And(f1, f2), And(f1, f3))
					|(Or(f2, f3), f1) -> Or(And(f1, f2), And(f1, f3))
					|_ -> p;;

to_dnf (nnf a6);;

(*****************************************MAIN FUNCTIONS**************************************************)


let rec cnf (p:prop) =
	let c = ClauseSet.empty 
	and litset = StringSet.empty
	and res = (to_cnf p) in

	match res with
	Atom s -> ClauseSet.add (StringSet.add s StringSet.empty) c
	|T -> ClauseSet.add (StringSet.add "T" StringSet.empty) c
	|F -> ClauseSet.add (StringSet.add "F" StringSet.empty) c
	|Or(p1,p2) -> ClauseSet.add (StringSet.union (getLiterals p1) (getLiterals p2)) c
	|And(p1,p2) -> ClauseSet.union (cnf p1) (cnf p2)
	|Not p1 -> match p1 with 
				Atom s -> ClauseSet.add (StringSet.add (String.concat " " ["Not";s]) StringSet.empty) c;;


(* print_set (getLiterals(Or (Not (Atom "pepo"), Or (Atom "B", Not (Atom "A")))));; *)
to_cnf (nnf a4);;
print_clause (cnf (nnf a6));;


let rec dnf (p:prop) =
	let c = ClauseSet.empty 
	and litset = StringSet.empty
	and res = (to_dnf p) in

	match res with
	Atom s -> ClauseSet.add (StringSet.add s StringSet.empty) c
	|T -> ClauseSet.add (StringSet.add "T" StringSet.empty) c
	|F -> ClauseSet.add (StringSet.add "F" StringSet.empty) c
	|Or(p1,p2) -> ClauseSet.union (dnf p1) (dnf p2)
	|And(p1,p2) -> ClauseSet.add (StringSet.union (getLiterals p1) (getLiterals p2)) c
	|Not p1 -> match p1 with 
				Atom s -> ClauseSet.add (StringSet.add (String.concat " " ["Not";s]) StringSet.empty) c;;


to_dnf (nnf a6);;
print_clause (dnf (nnf a6));;


