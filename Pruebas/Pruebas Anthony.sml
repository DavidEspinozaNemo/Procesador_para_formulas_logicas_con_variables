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

   Hay un caso para cada variante de proposicion.
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
(* Pruebas de corrida *)

val f = constante false;
val t = constante true;

val prop1 = f :=>: f :<=>: ~: f :=>: ~: f;
val prop2 = f :=>: f :<=>: ~: f :||: f;

val p = f;
val q = t;

val prop3 = p :=>: q :<=>: ~: p :||: q;
val prop4 = p :=>: q :<=>: ~: q :=>: ~: p;

***********************************************************************
(* ANTHONY ULLOA - 2018290801 *)
(* Funcion VARS - Datatype Arbol *)
datatype Arbol =
  Nulo
| Nodo of Arbol * Proposicion * Arbol
;
(* Intento de separacion - Resultado: Fallo *)
fun separacionX (x::xs, ch) = 
   if x = true then 
      1 + separacionX(xs,ch) 
   else 
      separacionX(xs,ch)
| separacionX  (_, ch) = 0;
fun separacion(s,ch) = cntcaux(String.explode s, ch);

(* Intento de creacion arbol - Resultado: Fallo *)
fun ins n Nulo               = Nodo (Nulo, n, Nulo)
|   ins n (Nodo (ai, m, ad)) =
      if n < m then
      	Nodo (ins n ai, m, ad)
      else if n > m then
      	Nodo (ai, m, ins n ad)
      else (* n = m *)
      	Nodo (ai, m, ad)
;

(* Otorgarle un numero a cada proposicion - Resultado: Inconcluso*)

fun poblar arb []        = arb
|   poblar arb (n :: ns) = poblar (ins n arb) ns
;

fun ird Nulo               = []
|   ird (Nodo (ai, m, ad)) = (ird ai) @  [m] @ (ird ad)
;

val prueba1 = poblar Nulo [10, 1, 5, 4 , 100, 0, 20, 4]
;

(* Verificacion de tipos - Resultado: Fallo*)

fun prueba n = 
if f = constante false then 
   n = true 
else if f = constante true then 
   n = true 
else 
   n = false;


fun vars
fun evalProp
fun gen_bools
fun as_vals
fun taut
fun fnd
fun simpl