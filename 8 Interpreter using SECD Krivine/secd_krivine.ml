type lambdaTerm = Var of string | Lm of string * lambdaTerm | App of lambdaTerm * lambdaTerm;;

type closure = Clos of table * lambdaTerm 
and table = Tab of (string * closure) list;;

let isValueClosure (Clos(t, l)) = match l with
                                Lm(x, l) -> true
                                |_ -> false;;

exception Nahi_pata;;

let rec map var tab = match tab with
                   Tab[] -> raise Nahi_pata
                   |Tab((xx, yy)::xxs) -> if var = xx then yy else map var (Tab(xxs));;                     



let krivine le = let rec kr (Clos(Tab(z), le)) st = match le with
                                           Var(x) -> let vc = map x (Tab(z)) in
                                            if (isValueClosure vc = true) then vc 
                                            else kr vc st
                                           |Lm(x, l) -> if not(Stack.is_empty st) then kr (Clos(Tab((x, (Stack.pop st))::z), l)) st
                                                           else (Clos(Tab(z), le))
                                           |App(e1, e2) -> Stack.push (Clos(Tab(z), e2)) st; kr (Clos(Tab(z), e1)) st
                           in let s = Stack.create()                
                           in kr (Clos(Tab[], le)) s;;

(*

let clo = Clos(Tab[],Lm("x",(Var("z"))));;

let clo2 = Clos(Tab["y",clo3],App(Lm("x",Var("x")), Var("y")));;
let clo3 = Clos(Tab[("y",clo)],App(Lm("x",Var("x")), Var("y")));;
let lc = App(Lm("x",Var("x")), Var("y"));;

let try1 = App(Lm("x", (Lm("y", (Var("x"))))), (Lm("y", (Var("y")))));;

let ak = App(Lm("y",App(Lm("z",Var("z")),Var("y"))),Lm("w",Var("w")));;
*)
let case = App(Lm("y", App(Lm("x", Lm ("y", Var "x")), Var "y")), Lm("y", Var "y"));;




* Clos
   (Tab
     [("x", Clos (Tab [("y", Clos (Tab [], Lm ("y", Var "y")))], Var "y"));
      ("y", Clos (Tab [], Lm ("y", Var "y")))],
   Lm ("y", Var "x"))


let a4 = krivine case;;  

