open Types

let get_ind_max (e:expr) : int =
	let rec aux e i_max  = match e with
	Var k -> max (abs k) i_max
	|Not(e) -> aux e i_max
	|And(e1,e2) -> let res1 = aux e1 i_max in
		let res2 = aux e2 i_max in
		max res1 res2
	|Or(e1,e2) -> let res1 = aux e1 i_max in
		let res2 = aux e2 i_max in
		max res1 res2
	|_ -> failwith "Erreur get_ind_max : prétraitement oublié !\n"
	
	in
	aux e 0


(* Formule de prétraitement pour simplifier la formule
Par exemple remplacer ~~A par A *)
let rec pretraitement_tseitin e : expr = match e with
	Var k -> Var k
	|Not(Not(e1)) -> pretraitement_tseitin e1
	|And(e1,e2) -> 
		if (e1 = e2) then
			pretraitement_tseitin e1
		else
			And(pretraitement_tseitin e1, pretraitement_tseitin e2)
	|Or(e1,e2) -> 
		if (e1 = e2) then
			pretraitement_tseitin e1
		else
			Or(pretraitement_tseitin e1, pretraitement_tseitin e2)
	|Xor(e1,e2) -> 
	(
		let form1 = And(e1,  Not(e2)) in
		let form2 = And(Not(e1), e2) in
		let new_expr = Or(form1,form2) in
		pretraitement_tseitin new_expr
	)
	|Impl(e1,e2) ->
	(
		let new_expr = Or(Not(e1),e2) in
		pretraitement_tseitin new_expr
	)
	|Equ(e1,e2) -> 
	(
		let aimpb = Impl(e1,e2) in
		let bimpa = Impl(e2,e1) in
		let new_expr = And(aimpb, bimpa) in
		pretraitement_tseitin new_expr
	)
	|Not(e1) -> Not(pretraitement_tseitin e1)
	



let tseitin (e:expr) (start_ind:int) : fnc =
	let current_indice = ref start_ind in 
	
	let get_new_var () = 
		current_indice := !current_indice +1;
		!current_indice -1
	in
	
	let rec aux e = match e with
	|Var k ->  (k, [])
	|Not p -> let (var, form) = aux p in
		(-1* var, form)
	|And(p1,p2) -> 
	(
		let new_var = get_new_var() in
		
		let (xip1, form1) = aux p1 in
		let (xip2, form2) = aux p2 in

		let clause1 = [-1*new_var; xip1] in
		let clause2 = [-1*new_var; xip2] in
		let clause3 = [new_var; -1*xip1; -1*xip2] in
		
		let form = form1@form2@[clause1;clause2;clause3] in
		
		(new_var,form)
	
	)
	|Or(p1,p2) -> 
	(
		let new_var = get_new_var() in
		
		let (xip1, form1) = aux p1 in
		let (xip2, form2) = aux p2 in

		let clause1 = [-1*xip1; new_var] in
		let clause2 = [-1*xip2; new_var] in
		let clause3 = [xip1; xip2; -1*new_var] in
		
		let form = form1@form2@[clause1;clause2;clause3] in
		
		(new_var,form)
	)
	|_ -> failwith "Erreur, prétraitement oublié !"
	in
	let (a,b) = aux e in
	[a]::b

	
