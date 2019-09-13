(************************************TYPE DECLARATION********************************************)
type prop = Atom of string
	|T |F
	|And of prop * prop
	|Not of prop
	|Or of prop * prop
	|Imply of prop * prop
	|Iff of prop * prop;;

let a0 = Atom "pepo";;
let a1 = T;;
let a2 = Not F;;
let a3 = Not a1;;
let a4 = And (a2,a1);;
let a5 = Imply (a0,a4);;
let a6 = Not a5;;

(**************************************HEIGHT******************************************)
let rec ht p = match p with 
	Atom s ->0
	|T -> 0
	|F-> 0
	|And(p1,p2) -> 1 + max (ht p1) (ht p2)
	|Or(p1,p2) -> 1 + max (ht p1) (ht p2)
	|Imply(p1,p2) -> 1 + max (ht p1) (ht p2)
	|Iff(p1,p2) -> 1 + max (ht p1) (ht p2)
	|Not p1 -> 1 + (ht p1);;

print_string("after ht declaration");;
ht a1;; 
ht a2;;
ht a3;;
ht a4;;


(**************************************SIZE******************************************)

let rec size p = match p with 
	Atom s -> 1
	|T -> 1
	|F -> 1
	|And(p1,p2) -> 1 +  (size p1) + (size p2)
	|Or(p1,p2) -> 1 +  (size p1) + (size p2)
	|Imply(p1,p2) -> 1 +  (size p1) + (size p2)
	|Iff(p1,p2) -> 1 +  (size p1) + (size p2)
	|Not p1 -> 1 + (size p1);;

print_string("check size");;
size a1;; 
size a2;;
size a3;;
size a4;;

(*************************************ATOMS*******************************************)
module StringSet  = Set.Make(String);;
module ClauseSet = Set.Make(StringSet);;

let print_set s = 
     StringSet.iter print_endline s;;

let rec atoms p = 
	let retset = StringSet.empty in 

	match p with
	Atom s -> StringSet.add s retset
	|T -> StringSet.add "T" retset
	|F -> StringSet.add "F" retset
	|And(p1,p2) -> StringSet.union (atoms p1) (atoms p2)
	|Or(p1,p2) -> StringSet.union (atoms p1) (atoms p2)
	|Imply(p1,p2) -> StringSet.union (atoms p1) (atoms p2)
	|Iff(p1,p2) -> StringSet.union (atoms p1) (atoms p2)
	|Not p1 -> (atoms p1);;

print_string("check atoms");;
print_set (atoms a1);; 
print_set (atoms a2);;
print_set (atoms a3);;
print_set (atoms a5);;


(***********************************TRUTH*********************************************)

type func = string -> bool;;

let rho s = match s with 
	"pepo" -> true
	|_ -> false;;

let rec truth (p:prop) (rho:func) : bool = match p with 
	Atom s -> rho s
	|T -> true
	|F -> false
	|And(p1,p2) -> (truth p1 rho) && (truth p2 rho)
	|Or(p1,p2) -> (truth p1 rho) || (truth p2 rho)
	|Imply(p1,p2) -> (not (truth p1 rho))  || (truth p2 rho)
	|Iff(p1,p2) -> ((not (truth p1 rho)) && (not (truth p2 rho))) || ((truth p1 rho) && (truth p2 rho))
	|Not p1 -> not (truth p1 rho);;

print_string("check truth");;

truth a1 rho;; 
truth a2 rho;;
truth a3 rho;;
truth a4 rho;;
truth a5 rho;;

(**********************************Helpler functions ************************************************************)


let rec atom_list p = match p with
    Atom s -> [s]
    | T -> []
    | F -> []
    | Not p1 -> atom_list p1
    | And (p1, p2) -> atom_list p1 @ atom_list p2   
    | Or (p1, p2) -> atom_list p1 @ atom_list p2 
    | Imply (p1, p2) -> atom_list p1 @ atom_list p2  
    | Iff (p1, p2) -> atom_list p1 @ atom_list p2 ;;



let rec generate_table fn fn1 ls = match ls with
[] -> fn fn1
|h::tl -> let fn2 a b = if b = h then a else fn1(b) in
			generate_table fn (fn2 false) tl && generate_table fn (fn2 true) tl;;


(**********************************Tautology, Satisfiability and others **********************************************)
let tautology p =
  generate_table (truth p) (fun x -> false) (atom_list p);;

let isSatisfiable p1 = not (tautology (Not p1));;


let isEquivalent p1 p2 = tautology (Iff(p1,p2));;


let entails p1 p2 = tautology (Imply(p1, p2));;



