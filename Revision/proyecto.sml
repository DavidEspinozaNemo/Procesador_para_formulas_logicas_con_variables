(* El proyecto es un conjunto de funciones, las pondre todas aqui para una revision ordenada. *)

(* Datatype *)
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

(* vars *)
fun filter p []      = []
|   filter p (x::xs) = if p x then x :: filter p xs else filter p xs
;

fun nub []      = []
|   nub (x::xs) = x :: (nub (filter (fn y => x <> y) xs))
;

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

(* eval_prop *)
fun last(xs) =
case xs of
    [] => raise List.Empty
    | (x::[]) => x
    | (_::xs') => last(xs')

fun get_value (nombreVariable: string, lista: (string*bool) list) =
    if null lista
    then false
    else if #1 (hd lista) = nombreVariable
    then #2 (hd lista)
    else get_value (nombreVariable, tl lista);

fun eval_prop (prop, contexto) =
  case prop of
   constante valor
       => valor
  |
    variable valor
       => get_value(valor, contexto)
  | negacion prop1
       => not (eval_prop (prop1, contexto))
  | conjuncion (prop1, prop2)
       => let val valor1 = eval_prop (prop1, contexto)
              and valor2 = eval_prop (prop2, contexto)
          in  valor1 andalso valor2
          end
  | disyuncion (prop1, prop2)
       => let val valor1 = eval_prop (prop1, contexto)
              and valor2 = eval_prop (prop2, contexto)
          in  valor1 orelse valor2
          end
  | implicacion (prop1, prop2)
       => let val valor1 = eval_prop (prop1, contexto)
              and valor2 = eval_prop (prop2, contexto)
          in  case (valor1, valor2) of
                (true, false) => false
              | _             => true
          end
  | equivalencia (prop1, prop2)
       => let val valor1 = eval_prop (prop1, contexto)
              and valor2 = eval_prop (prop2, contexto)
          in  valor1 = valor2
          end
;

(* gen_bools *)
fun add [x] xs = [x] @ xs;

fun map f [] = []
| map f (x::xs) = f(x) :: map f xs;

fun gen_bools 1 = [[true, false]]
|   gen_bools 2 = [[true, true], [true, false], [false, true], [false, false]]
|   gen_bools n = (map (add [true] ) (gen_bools (n-1))) @ (map (add [false] ) (gen_bools (n-1)));

(* as_vals *)
fun addLista []    []    = []
|   addLista (x)   (y)   = (x) @ (y)
;

fun as_val_aux []        []               = []
|   as_val_aux (x :: xs) (y :: ys)        = (x, y) :: as_val_aux xs ys
|   as_val_aux []        _                = []
|   as_val_aux _         _                = []
;

fun as_val   []          []               = []
|   as_val   (x :: xs)   ((y::js) :: ys)  = addLista [(x, y)] (as_val_aux xs js) :: as_val (x :: xs) ys
|   as_val   []          _                = []
|   as_val   _           _                = []
;

(* taut *)
fun print_tuple (x,y) =
if y = true
then x ^ " = true"
else x ^ " = false";

fun print_context [] = " "
| print_context (x::xs) = print_tuple x ^ ", " ^ print_context xs;

fun cont_vars [] = 0
| cont_vars (x::xs) = 1 + cont_vars xs;

fun aux_taut_logic prop [] = "Es una Tautologia"
| aux_taut_logic prop (x::xs) = 
	if (eval_prop (prop, x) )
	then aux_taut_logic prop xs
	else "No es Tautologia por que; " ^ print_context x ^ " hacen falsa la propocicion."
	
fun aux_taut prop =
	if (cont_vars (vars prop)) = 0
	then aux_taut_logic prop [[("exception",true)]]
	else aux_taut_logic prop (as_val (vars prop) (gen_bools(cont_vars (vars prop))) );
	
fun taut prop = aux_taut prop;

(* simpl *)
fun cont_elem_prop prop = 
	case prop of
		constante valor
			=> 1
	|	variable valor
			=> 1
	|	conjuncion (prop1, prop2) 
			=> 1 + cont_elem_prop prop1 + cont_elem_prop prop2
	|   disyuncion (prop1, prop2)
			=> 1 + cont_elem_prop prop1 + cont_elem_prop prop2
	|	negacion prop1
			=> 1 + cont_elem_prop prop1
	|	implicacion (prop1, prop2)
			=> 1 + cont_elem_prop prop1 + cont_elem_prop prop2
	|	equivalencia (prop1, prop2)
			=> 1 + cont_elem_prop prop1 + cont_elem_prop prop2
;
(* -> neutro *)
fun isVariable prop =
	case prop of	
		variable _  => true
	|	         _  => false
;

fun neutro prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	conjuncion (prop1, constante true) 
			=> neutro prop1
	|	conjuncion (constante true, prop2) 
			=> neutro prop2
	|	conjuncion (prop1, prop2) 
			=> conjuncion (neutro prop1, neutro prop2)
	|   disyuncion (prop1, constante false)
			=> neutro prop1
	|   disyuncion (constante false, prop2)
			=> neutro prop2
	|	disyuncion (prop1, prop2) 
			=> disyuncion (neutro prop1, neutro prop2)
	|	negacion prop1
			=> negacion (neutro prop1) 
	|	implicacion (constante true, prop2)
			=> neutro prop2
	|	implicacion (prop1, prop2)
			=> implicacion (neutro prop1, neutro prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (neutro prop1, neutro prop2)
;

(* -> idempotencia *)
fun idempotencia prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	conjuncion ( prop1 , prop2 ) =>
			if (prop1 = prop2)
			then idempotencia prop1
			else conjuncion (idempotencia prop1, idempotencia prop2)
	|	disyuncion ( prop1 , prop2 ) =>
			if (prop1 = prop2)
			then idempotencia prop1
			else disyuncion (idempotencia prop1, idempotencia prop2)
	|	negacion prop1
			=> negacion (idempotencia prop1) 
	|	implicacion (prop1, prop2)
			=> implicacion (idempotencia prop1, idempotencia prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (idempotencia prop1, idempotencia prop2)
;

(* -> doble negacion *)

fun doble_neg prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	negacion( negacion( prop1))
			=> doble_neg prop1
	|	negacion prop1
			=> negacion (doble_neg prop1)
	|	conjuncion (prop1, prop2)
			=> conjuncion (doble_neg prop1, doble_neg prop2)
	|   disyuncion (prop1, prop2)
			=> disyuncion (doble_neg prop1, doble_neg prop2)
	|	implicacion (prop1, prop2)
			=> implicacion (doble_neg prop1, doble_neg prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (doble_neg prop1, doble_neg prop2)
;

(* -> distributiva *)
fun dis prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	negacion prop1
			=> negacion (dis prop1)
	|   conjuncion (disyuncion (prop1, prop2), disyuncion (prop3, prop4))
			=> if prop1 = prop3 then
			   		disyuncion (dis prop1, conjuncion(dis prop2, dis prop4))
			   else
			   		conjuncion (disyuncion (dis prop1, dis prop2), disyuncion (dis prop3, dis prop4))
    |	disyuncion (conjuncion (prop1, prop2), conjuncion (prop3, prop4))
			=> if prop1 = prop3 then
					conjuncion (dis prop1, disyuncion(dis prop2, dis prop4))
			   else
			   		disyuncion (conjuncion (dis prop1, dis prop2), conjuncion (dis prop3, dis prop4))
	|	implicacion (prop1, prop2)
			=> implicacion (dis prop1, dis prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (dis prop1, dis prop2)
	|	_
			=> prop
;

(* -> De Morgan *)
fun de_morgan prop = 
	case prop of 
		constante valor
			=> constante valor
		|	variable valor
			=> variable valor
        |	negacion prop1
            => ( case prop1 of         
              	conjuncion(prop1, prop2) => disyuncion(negacion(de_morgan(prop1)), negacion (de_morgan(prop2)))
               | disyuncion(prop1, prop2) => conjuncion(negacion(de_morgan(prop1)), negacion (de_morgan(prop2)))
			   | _ => negacion (de_morgan prop1)
			   )
		|	conjuncion (negacion prop1, negacion prop2)
			=> negacion (disyuncion (de_morgan prop1, de_morgan prop2))
		|   disyuncion (negacion prop1, negacion prop2)
			=> negacion (conjuncion (de_morgan prop1, de_morgan prop2))
		|	implicacion (prop1, prop2)
			=> implicacion (de_morgan prop1, de_morgan prop2)
		|	equivalencia (prop1, prop2)
			=> equivalencia (de_morgan prop1, de_morgan prop2)
		|	conjuncion(prop1, prop2)
			=> negacion(disyuncion(negacion(de_morgan (prop1)), negacion(de_morgan (prop2))))
		|	disyuncion(prop1, prop2)
			=> negacion(conjuncion(negacion(de_morgan (prop1)), negacion(de_morgan (prop2))))
;

(* -> Conmutativa *)
fun com prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	negacion prop1
			=> negacion (com prop1)
	|	conjuncion (prop1, prop2)
			=> conjuncion (com prop2, com prop1)
	|   disyuncion (prop1, prop2)
			=> disyuncion (com prop2, com prop1)
	|	implicacion (prop1, prop2)
			=> implicacion (com prop1, com prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (com prop1, com prop2)
;

(* -> Asociativa *)
fun aso prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	negacion prop1
			=> negacion (aso prop1)
    |	conjuncion (conjuncion (prop1, prop2), prop3)
			=> conjuncion (aso prop1, conjuncion (aso prop2, aso prop3))
	|	conjuncion (prop1, conjuncion (prop2, prop3))
			=> conjuncion (conjuncion (aso prop1, aso prop2), aso prop3)
    |   disyuncion (disyuncion (prop1, prop2), prop3)
            => disyuncion (aso prop1, disyuncion (aso prop2, aso prop3))
	|   disyuncion (prop1, disyuncion (prop2, prop3))
			=> disyuncion (disyuncion (aso prop1, aso prop2), aso prop3)
	|	implicacion (prop1, prop2)
			=> implicacion (aso prop1, aso prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (aso prop1, aso prop2)
	|	_
			=> prop
;

(* simpl *)
fun simpl prop = 
    aso(com(de_morgan(dis(doble_neg(idempotencia(neutro(prop)))))))
;
