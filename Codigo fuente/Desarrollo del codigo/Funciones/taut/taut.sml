(* funcion taut *)
(* David Espinoza *)

(* Son tres funciones que evaluan la propocicion *)
(* NO puedo probarlo sin antes poner las otras funciones *)

(* Funcion para imprimir los valores de las tuplas en caso de no ser tautologia *)
fun print_tuple (x,y) =
if y = true
then x ^ " = true"
else x ^ " = false";

fun print_context [] = " "
| print_context (x::xs) = print_tuple x ^ ", " ^ print_context xs;

(* necesito una funcion para saber cuantas variables ahi *)
fun cont_vars [] = 0
| cont_vars (x::xs) = 1 + cont_vars xs;

(* actualización de evalProp funcional, echa por otro compañero *)
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
	
	
fun evalProp (prop, contexto) =
  case prop of
   constante valor
        => valor
  |
    variable valor
        => get_value(valor, contexto)
  | negacion prop1
       => not (evalProp (prop1, contexto))
  | conjuncion (prop1, prop2)
       => let val valor1 = evalProp (prop1, contexto)
              and valor2 = evalProp (prop2, contexto)
          in  valor1 andalso valor2
          end
  | disyuncion (prop1, prop2)
       => let val valor1 = evalProp (prop1, contexto)
              and valor2 = evalProp (prop2, contexto)
          in  valor1 orelse valor2
          end
  | implicacion (prop1, prop2)
       => let val valor1 = evalProp (prop1, contexto)
              and valor2 = evalProp (prop2, contexto)
          in  case (valor1, valor2) of
                (true, false) => false
              | _             => true
          end
  | equivalencia (prop1, prop2)
       => let val valor1 = evalProp (prop1, contexto)
              and valor2 = evalProp (prop2, contexto)
          in  valor1 = valor2
          end
;

(* fun aux_taut_logic *)
fun aux_taut_logic prop [] = "Es una Tautologia"
| aux_taut_logic prop (x::xs) = 
	if (evalProp (prop, x) )
	then aux_taut_logic prop xs
	else "No es Tautologia por que; " ^ print_context x ^ " hacen falsa la propocicion."

(* fun aux_taut proporciona un ambiente *)

fun aux_taut prop =
	if (cont_vars (vars prop)) = 0
	then aux_taut_logic prop [[("exception",true)]]
	else aux_taut_logic prop (as_val (vars prop) (gen_bools(cont_vars (vars prop))) );

(* funcion taut *)
fun taut prop = aux_taut prop;