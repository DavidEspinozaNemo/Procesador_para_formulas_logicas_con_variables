(* ANTHONY ULLOA - 2018290801 *)
(* Pruebas de Codigo *)
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

***********************************************************************
(* Pruebas de as_val *)
fun zipT []            []            = []
|   zipT ([x,z] :: xs) ([y,w] :: ys) = [(x, y),(z, w)] :: zipT xs ys
|   zipT []            _             = [] 
|   zipT _             _             = [] 
;

fun zipR []        []            = [] 
|   zipR (x :: xs) ([y,w] :: ys) = [(x, y),(x, w)] :: zipR xs ys
|   zipR []        _             = [] 
|   zipR _         _             = []
;

fun zip1 []       []          = [] 
|   zip1 ([x])    ([y] :: ys) = (x, y) :: zip1 [x] ys (* Para gen_bool(1) *)
|   zip1 []       _           = [] 
|   zip1 _        _           = [] 
;

fun zip2 []        []            = []
|   zip2 ([x, z])  ([y,w] :: ys) = [(x, y),(z, w)] :: zip2 [x, z] ys (* Para gen_bool(2) *)
|   zip2 []        _             = [] 
|   zip2 _         _             = []
;

fun zip3 []          []                  = []
|   zip3 ([x])       ([y] :: ys)         = [(x, y)] :: zip3 [x] ys (* Para gen_bool(1) *)
|   zip3 ([x, z])    ([y, w] :: ys)      = [(x, y),(z, w)] :: zip3 [x, z] ys (* Para gen_bool(2) *)
|   zip3 ([x, z, p]) ([y, w , k] :: ys)  = [(x, y),(z, w),(p, k)] :: zip3 [x, z, p] ys (* Para gen_bool(3) *)
|   zip3 []          _                   = [] 
|   zip3 _           _                   = []
;

fun zipP []        []        = []
|   zipP (x :: xs) (y :: ys) = (x, y) :: zipP xs ys
|   zipP []        _         = [] 
|   zipP _         _         = [] 
;

(* Resultado de zipT *)
(* Se obtiene este resultado favorable debido a que se usa una lista de variables Trucada *)
(* Se logro usando zipT lista0 lista4; *)
[
   [("p", false), ("q", false)], 
   [("p", true),  ("q", false)],
   [("p", false), ("q", true) ], 
   [("p", true),  ("q", true) ]
]

(* Resultado de zipR *)
(* Se obtiene este resultado erroneo debido a que la lista Repite el valor *)
(* Se logro usando zipR lista3 lista4; *)
[
   [("p", false), ("p", false)], 
   [("q", true),  ("q", false)]
]

(* Resultado de zip1 *)
(* Se obtiene este resultado favorable para gen_bool(1) *)
(* Se logro usando zip1 lista1 lista2; *)
[
   [("p", false)], 
   [("p", true)]
]

(* Resultado de zip2 *)
(* Se obtiene este resultado favorable con gen_bool(2) *)
(* Se logro usando zip2 lista3 lista4; *)
[
   [("p", false), ("q", false)], 
   [("p", true),  ("q", false)],
   [("p", false), ("q", true) ], 
   [("p", true),  ("q", true) ]
]