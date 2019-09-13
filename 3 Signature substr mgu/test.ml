(************************************TYPE DECLARATION********************************************)
type prop = Atom of string
	|T |F
	|And of prop * prop
	|Not of prop
	|Or of prop * prop
	|Imply of prop * prop
	|Iff of prop * prop;;

let rec atom_list p = match p with
    Atom s -> [s]
    | T -> []
    | F -> []
    | Not p1 -> atom_list p1
    | And (p1, p2) -> atom_list p1 @ atom_list p2   
    | Or (p1, p2) -> atom_list p1 @ atom_list p2 
    | Imply (p1, p2) -> atom_list p1 @ atom_list p2  
    | Iff (p1, p2) -> atom_list p1 @ atom_list p2 ;;

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


let rec generate_table fn fn1 ls = match ls with
[] -> fn fn1
|h::tl -> let fn2 a b = if b = h then a else fn1(b) in
			generate_table fn (fn2 false) tl && generate_table fn (fn2 true) tl;;

let tautology p =
  generate_table (truth p) (fun x -> false) (atom_list p);;

let isSatisfiable p1 = not (tautology (Not p1));;


let isEquivalent p1 p2 = tautology (Iff(p1,p2));;


  (* ATM <TICKET NO> 56767 ...   *)