(* Valores semánticos.
   Nuestro lenguaje es muy sencillo, solo admite literales booleanas *)

type Val = bool;

***********************************************************************
(* Lenguaje de proposiciones con constantes.*)

(* Aqui definimos la sintaxis abstracta de nuestro pequenno
   lenguaje de proposiciones con constantes *)

datatype Proposicion =
constante of bool
| variable of string
| negacion of Proposicion
| conjuncion of Proposicion * Proposicion
| disyuncion of Proposicion * Proposicion
| implicacion of Proposicion * Proposicion
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

***********************************************************************
(* Evaluador de proposiciones.

   Hay un caso para cada variante de proposici�n.
*)

fun evalProp prop =
  case prop of
    constante valor
       => valor
  | negacion prop1
       => not (evalProp prop1)
  | conjuncion (prop1, prop2)
       => let val valor1 = evalProp prop1
              and valor2 = evalProp prop2
          in  valor1 andalso valor2
          end
  | disyuncion (prop1, prop2)
       => let val valor1 = evalProp prop1
              and valor2 = evalProp prop2
          in  valor1 orelse valor2
          end
  | implicacion (prop1, prop2)
       => let val valor1 = evalProp prop1
              and valor2 = evalProp prop2
          in  case (valor1, valor2) of
                (true, false) => false
              | _             => true
          end
  | equivalencia (prop1, prop2)
       => let val valor1 = evalProp prop1
              and valor2 = evalProp prop2
          in  valor1 = valor2
          end
;

***********************************************************************
(* pruebas *)

val f = constante false
val t = constante true

val prop1 = f :=>: f :<=>: ~: f :=>: ~: f
val prop2 = f :=>: f :<=>: ~: f :||: f
;

val p = f;
val q = t;

val prop3 = p :=>: q :<=>: ~: p :||: q
val prop4 = p :=>: q :<=>: ~: q :=>: ~: p
;

***********************************************************************
(* Funciones *)

fun cntcaux (h::xs, ch) = if h = ch then 1 + cntcaux(xs,ch) else cntcaux(xs,ch)
  | cntcaux  (_, ch) = 0;
fun cntc(s,ch) = cntcaux(String.explode s, ch);

fun vars
fun evalProp
fun gen_bools
fun as_vals
fun taut
fun fnd
fun simpl