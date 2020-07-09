(* Jasson Gonzalez Torrez*)
(* Funcion eval_prop *)

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
val (op :<=>:) = equivalencia;

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

get_value("r", [("p", false), ("q",true), ("r",false)]);
get_value("q", [("p", false), ("q",true), ("r",true)]);

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

val p = variable "p";
val q = variable "q";

val prop1 = p :&&: q :||: p;
evalProp (prop1, [("p", true), ("q", true)]);
evalProp (prop1, [("p", false), ("q", true)]);