(* El tipo de dato con el que trabajaremos *)

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
val f = constante false;
val t = constante true;
val prop1 = p :=>: q :<=>: ~: p :||: q;
val prop2 = f :=>: p :<=>: q :=>: ~: f;