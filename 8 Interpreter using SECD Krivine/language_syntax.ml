type integer = int;;
type boolean = bool;;

type const = Int of integer
			| Bool of boolean
			| Unit
;;

type variable = string;;

type program = C of const
			  | V of variable
			  | Add of program*program
			  | Mul of program*program
			  | Sub of program*program
			  | Div of program*program
			  | And of program*program
			  | Or of program*program
			  | Not of program
			  | Equals of program*program
			  | Elif of program*program*program    (*Type check comparision condition here ?? *)
			  | Tup of program list
			  | Proj of (program -> program)
			  | Abs of variable * program     (*Passing a variable instead of variable list here makes our code curried ??? *)
			  | App of program * program
;;

type opcode = CONST of const
			 | LOOKUP of variable
			 | PLUS 
			 | MUL
			 | MINUS
			 | DIV
			 | AND 
			 | OR
			 | NOT
			 | EQUALS
			 | ELIF of opcode list * opcode list
			 | TUP
			 | PROJ
			 | CLOS of variable * opcode list
			 | CALL
			 | RET
;;




type env = (variable * const) ;;  (*sHOULD be (var *const )list*)

let (stk_env : env Stack.t) = Stack.create();;
(* let ls_env = env list;; *)

type closure = variable * (opcode list) * (env list);;

let (stk_c : const Stack.t) = Stack.create();;

let (stk_clos : (closure) Stack.t) = Stack.create ();;

type code = opcode list;;

type dump = ((const Stack.t) * (closure Stack.t) * (env list) * code);;

let (stk_dmp : dump Stack.t) = Stack.create();;
(* let ls_dmp = dump ;; *)


let rec compile (p:program) : code = 
	match  p  with
	C x -> [CONST x]
	| V x -> [LOOKUP x]
	| Add (p1,p2) -> compile(p1) @ compile(p2) @ [PLUS]
	| Mul (p1,p2) -> compile(p1) @ compile(p2) @ [MUL]
	| Sub (p1,p2) -> compile(p1) @ compile(p2) @ [MINUS]
	| Div (p1,p2) -> compile(p1) @ compile(p2) @ [DIV]
	| And (p1,p2) -> compile(p1) @ compile(p2) @ [AND]
	| Or (p1,p2) -> compile(p1) @ compile(p2) @ [OR]
	| Not p1 -> compile(p1) @ [NOT]
	| Equals (p1,p2) -> compile(p1) @ compile(p2) @ [EQUALS]
	| Elif (p1,p2,p3) -> compile(p1) @ [ELIF ( compile(p2), compile(p3) ) ]
	| Tup p_ls -> []
	| Proj funct -> []
	| Abs (x,p1) -> [CLOS ( x ,compile(p1) @ [RET] ) ]
	| App (p1,p2) -> compile(p1) @ compile(p2) @ [CALL]
;;

exception Error of string;;


let rec exec (stk_c:const Stack.t) (stk_clos : (closure) Stack.t) (c:code) (ls_env:env list) (ls_dmp:dump list) : const = 
	match c with 
	[] -> Stack.pop stk_c
	| h::tl -> match h with 
				CONST c -> let fool = Stack.push c stk_c in 
							 (exec stk_c stk_clos tl ls_env ls_dmp)

				| LOOKUP x -> (try 
								 match (List.find (fun a -> (match a with (p,q) -> (p==x) )) ls_env) with 
									 (p,q) -> let fool = Stack.push q stk_c in
									 		  (exec stk_c stk_clos tl ls_env ls_dmp)
							with 
							  	 Not_found -> raise (Error "Variable not found in environment"))


				| PLUS -> (let first = Stack.pop stk_c
						  and second = Stack.pop stk_c in 
						  match first with 
						  Int one -> match second with 
							  			 Int sec -> let fool = Stack.push (Int (one + sec)) stk_c in
							  			 			(exec stk_c stk_clos tl ls_env ls_dmp)
							  			 | _ -> raise (Error "No int in second while plus")
						  | _ -> raise (Error "No int in first while plus"))

				| MUL ->  (let first = Stack.pop stk_c
						  and second = Stack.pop stk_c in 
						  match first with 
						  Int one -> match second with 
							  			 Int sec -> let fool = Stack.push (Int (one * sec)) stk_c in 
							  			 			(exec stk_c stk_clos tl ls_env ls_dmp)
							  			 | _ -> raise (Error "No int in second while MUL")
						  | _ -> raise (Error "No int in first while MUL"))

				| MINUS -> (let first = Stack.pop stk_c
						   and second = Stack.pop stk_c in 
						   match first with 
						   Int one -> match second with 
							  			 Int sec -> let fool = Stack.push (Int (sec - one)) stk_c in 
							  			 			(exec stk_c stk_clos tl ls_env ls_dmp)
							  			 | _ -> raise (Error "No int in second while Minus")
						  | _ -> raise (Error "No int in first while Minus"))

				| DIV ->  (let first = Stack.pop stk_c
						  and second = Stack.pop stk_c in 
						  match first with 
						  Int one -> match second with 
							  			 Int sec -> let fool = Stack.push (Int (sec/one)) stk_c  in 
							  			 			(exec stk_c stk_clos tl ls_env ls_dmp)
							  			 | _ -> raise (Error "No int in second while Div")
						  | _ -> raise (Error "No int in first while Div"))

				| AND -> (let first = Stack.pop stk_c
						  and second = Stack.pop stk_c in 
						  match first with 
							  Bool one -> match second with 
								  			 Bool sec -> let fool = Stack.push (Bool (one && sec)) stk_c in 
								  			 			 (exec stk_c stk_clos tl ls_env ls_dmp)
								  			 | _ -> raise (Error "No Bool in second while and")
							  | _ -> raise (Error "No Bool in first while and"))

				| OR -> (let first = Stack.pop stk_c
						  and second = Stack.pop stk_c in 
						  match first with 
							  Bool one -> match second with 
								  			 Bool sec -> let fool = Stack.push (Bool (one || sec)) stk_c in 
								  			 			(exec stk_c stk_clos tl ls_env ls_dmp)
								  			 | _ -> raise (Error "No Bool in second while or")
							  | _ -> raise (Error "No Bool in first while or"))

				| NOT -> (let first = Stack.pop stk_c in 
						  match first with 
							  Bool one -> let fool = Stack.push (Bool (not one)) stk_c in 
							  			  (exec stk_c stk_clos tl ls_env ls_dmp)
							  | _ -> raise (Error "No Bool in stack while not"))

				| EQUALS -> (let first = Stack.pop stk_c
						    and second = Stack.pop stk_c in 
						    match (first,second) with
							  	(Int a,Int b) -> let fool = Stack.push (Bool (a = b)) stk_c in 
							  				   (exec stk_c stk_clos tl ls_env ls_dmp)
							  	| (Bool a,Bool b) -> let fool = Stack.push (Bool (a = b)) stk_c in 
							  					   (exec stk_c stk_clos tl ls_env ls_dmp)
							  	| (Unit, Unit) -> let fool = Stack.push (Bool true) stk_c in 
							  				    (exec stk_c stk_clos tl ls_env ls_dmp)
							  	| (_ , _) -> let fool = Stack.push (Bool false) stk_c  in 
							  			   (exec stk_c stk_clos tl ls_env ls_dmp))

				| ELIF (c1,c2) -> (let first = Stack.pop stk_c in 
								  match first with 
									  Bool a -> (if a then (exec stk_c stk_clos (c1 @ tl) ls_env ls_dmp) else (exec stk_c stk_clos (c2 @ tl) ls_env ls_dmp))
									  | _ -> raise (Error "No Bool in stack while elif"))

				| CLOS (v,c1) -> ( let fool = Stack.push (v,c1,ls_env) stk_clos in 
									(exec stk_c stk_clos tl ls_env ls_dmp))

				| CALL -> ( let first = Stack.pop stk_c 
							and second = Stack.pop stk_clos in
							match first with 
							Int _
							| Bool _ 
							| Unit -> match second with 
														(a,b,c) -> (let not_fool = (((Stack.copy stk_c),(Stack.copy stk_clos),ls_env,tl) :: ls_dmp) in 
																	let fool = Stack.clear stk_c in 
																	let fool2 = Stack.clear stk_clos in 
																	(exec stk_c stk_clos b ((a,first)::c) not_fool)
																	)
														| _ -> raise (Error "Second mismatch in CALL")
							| _ -> raise (Error "First mismatch in CALL") )

				| RET -> ( match ls_dmp with
							  [] -> raise (Error "Dump empty while RET")
							  |x::xs -> match x with 
							  				(a,b,c,d) -> let not_fool = Stack.pop stk_c in 
							  							 let fool = Stack.push not_fool a in 
							  							 (exec a b d c xs)
							  				| _ -> raise (Error "Dump is not a quadruple")
						 )

;;

(* 

let myprog1 = App( Abs("x",Equals(V "x",Div(C (Int 10),C (Int 5)) ) ), App( Abs("x",C (Int 3)) , Elif( Equals(C (Int 5),C (Int 5)), Not (C (Bool true)) , C (Bool true) ) ) );;
let mycode1 = compile myprog1 ;;

exec stk_c stk_clos [
	CLOS ("x", [LOOKUP "x"; CONST (Int 10); CONST (Int 5); DIV; EQUALS; RET]);

    CLOS ("x", [CONST (Int 3); RET]);
	CONST (Int 5); 
	CONST (Int 5);
	EQUALS;
    ELIF ([CONST (Bool true); NOT], [CONST (Bool true)]);
    CALL] [] [];;



 *)

(* 
let myprog2 = App( Abs( "y", Add(V "x",V "y") ) , Abs("x", C (Int 3) ) );;
let mycode2 = compile myprog2;;
exec stk_c stk_clos mycode2 [] [];; *)

(* 

let myprog4 = App( (Abs("f",App(V "f",C (Int 2)))) , Abs("y",Add(V "y",C (Int 3))) );;
let mycode4 = compile myprog4;;
exec stk_c stk_clos mycode4 [] [];;

let myprog5 = App( Abs("f",(C (Int 2)) ) , Abs("y", Tup([(C (Int 3));(C (Int 5));(C (Int 6))])) ) ;;
let mycode5 = compile myprog5;;
exec stk_c stk_clos mycode5 [] [];;


app(app(lam(x, lam(y, x + y)), 2), 3)

let my

 *)
(* let myprog1 = App(Abs("x",Equals(V "x",Div(C (Int 10),C (Int 5)) ) ), Abs("x",C (Int 3))) , Elif( Equals(C (Int 5),C (Int 5)), Not (C (Bool true)) , C (Bool true) ) ) );;
let mycode1 = compile myprog1 ;; *)

exec stk_c stk_clos [
	CLOS ("x", [LOOKUP "x"; CONST (Int 10); CONST (Int 5); DIV; EQUALS; RET]);

    CLOS ("x", [CONST (Int 3); RET]);
	CONST (Int 5); 
	CONST (Int 5);
	EQUALS;
    ELIF ([CONST (Bool true); NOT], [CONST (Bool true)]);
    CALL] [] [];;