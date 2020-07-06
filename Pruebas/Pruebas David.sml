(* Prueba de tautologia *)
(* Funcion vars *)
datatype Proposicion =
   constante   of bool
|  variable    of string
|  negacion    of Proposicion
|  conjuncion  of Proposicion * Proposicion
|  disyuncion  of Proposicion * Proposicion
|  implicacion of Proposicion * Proposicion
| equivalencia of Proposicion * Proposicion
;

nonfix ~:
val ~: = negacion

infix 7 :&&:
val (op :&&:) = conjuncion

infix 6 :||:
val (op :||:) = disyuncion

infix 5 :=>:
val (op :=>:) = implicacion

infix 4 :<=>:
val (op :<=>:) = equivalencia

;
(* El ejemplo de entrada *)
val p = variable "p";
val q = variable "q";
val r = variable "r";
val f = constante false;
val t = constante true;
val prop1 = p :=>: q :<=>: ~: p :||: q;
val prop2 = f :=>: p :<=>: q :=>: ~: f;
val prop3 = r :=>: p :&&: q :<=>: r :||: q :=>: ~: r;
(* funciones auxiliares *)
(* filter filtra una lista de acuerdo con un predicado p *)

fun filter p []      = []
|   filter p (x::xs) = if p x then x :: filter p xs else filter p xs
;


(* nub obtiene una lista sin duplicados a partir de una lista arbitraria *)
fun nub []      = []
|   nub (x::xs) = x :: (nub (filter (fn y => x <> y) xs))
;

(* vars *)
type str = string;

fun aux_vars prop =
	case prop of
		constante valor
			=> [ ]
	|	variable valor
			=> [valor]
	|	negacion prop1
			=> aux_vars prop1
	|	conjuncion (prop1, prop2)
			=> let val valor1 = aux_vars prop1
				   and valor2 = aux_vars prop2
			   in valor1 @ valor2
			   end
	|   disyuncion (prop1, prop2)
			=> let val valor1 = aux_vars prop1
				   and valor2 = aux_vars prop2
			   in valor1 @ valor2
			   end
	|	implicacion (prop1, prop2)
			=> let val valor1 = aux_vars prop1
				   and valor2 = aux_vars prop2
			   in valor1 @ valor2
			   end
	|	equivalencia (prop1, prop2)
			=> let val valor1 = aux_vars prop1
				   and valor2 = aux_vars prop2
			   in valor1 @ valor2
			   end
;

fun vars prop = nub (aux_vars prop);

(* gen_bools opcional *)

fun add_2 [x] xs = [x] @ xs;

fun map f [] = []
| map f (x::xs) = f(x) :: map f xs;


(* - map (add [true] ) [[true, true], [false, false]];                    *)
(* > val it = [[true, true, true], [true, false, false]] : bool list list *)

fun gen_bools 1 = [[true, false]]
|   gen_bools 2 = [[true, true], [true, false], [false, true], [false, false]]
|   gen_bools n = (map (add_2 [true] ) (gen_bools (n-1))) @ (map (add_2 [false] ) (gen_bools (n-1)));

(* as_val *)
fun add []    []    = []
|   add (x)   (y)   = (x) @ (y)
;

fun as_val_aux []        []               = []
|   as_val_aux (x :: xs) (y :: ys)        = (x, y) :: as_val_aux xs ys
|   as_val_aux []        _                = []
|   as_val_aux _         _                = []
;

fun as_val   []          []               = []
|   as_val   (x :: xs)   ((y::js) :: ys)  = add [(x, y)] (as_val_aux xs js) :: as_val (x :: xs) ys
|   as_val   []          _                = []
|   as_val   _           _                = []
;

(* taut *)

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