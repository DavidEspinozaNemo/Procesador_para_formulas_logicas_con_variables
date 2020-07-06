(* funcion taut *)
(* David Espinoza *)

(* Son tres funciones que evaluan la propocicion *)
(* NO puedo probarlo sin antes poner las otras funciones *)

(* necesito una funcion para saber cuantas variables ahi *)
fun cont_vars [] = 0
| cont_vars (x::xs) = 1 + cont_vars xs;

(* para poder probar hare un evalProp falso *)
fun evalProp prop (x::xs) = true;

(* fun aux_taut_logic *)
fun aux_taut_logic prop [] = "Es una Tautologia"
| aux_taut_logic prop (x::xs) = 
	if (evalProp prop x) = true
	then aux_taut_logic prop xs
	else "No es Tautologia"

(* fun aux_taut proporciona un ambiente *)

fun aux_taut prop = aux_taut_logic prop (as_val (vars prop) (gen_bools(cont_vars (vars prop))) );

(* funcion taut *)
fun taut prop = aux_taut prop;