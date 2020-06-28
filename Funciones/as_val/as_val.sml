(* ANTHONY ULLOA - 2018290801 *)
(* Funcion AS_VAL *)
(* Defina una función, as_vals que, dada una lista de variables proposicionales sin repeticiones, la combina con
una lista de valores booleanos (true o false) de la misma longitud, para producir una lista del tipo
(string * bool) list que combina, posicionalmente, cada variable proposicional con el
correspondiente valor booleano. Esto lo denominamos una asignación de valores (a las variables
proposicionales). *)

(* Entrada: ["p","q"] *)

fun zipM []        []            = []
|   zipM ([x,z] :: xs) ([y,w] :: ys) = [(x, y),(z, w)] :: zipM xs ys
|   zipM []        _             = [] (* paramos porque xs es más corta que ys *)
|   zipM _         _             = [] (* paramos porque ys es más corta que xs *)
;

fun zipL []        []            = []
|   zipL (x :: xs) ([y,w] :: ys) = [(x, y),(x, w)] :: zipL xs ys
|   zipL []        _             = [] (* paramos porque xs es más corta que ys *)
|   zipL _         _             = [] (* paramos porque ys es más corta que xs *)
;

fun as_val []        []            = []
|   as_val ([x, z]) ([y,w] :: ys) = [(x, y),(z, w)] :: as_val [x, z] ys
|   as_val []        _             = [] 
|   as_val _         _             = []
;

val lista0  = ["p"];
val lista1  = [[false,true]];

val lista2  = ["p","q"];

val lista3  = [["p","q"],["p","q"],["p","q"],["p","q"]];
val lista4  = [[false,false],[true,false],[false,true],[true,true]];

zipL lista0 lista1;
zipL lista2 lista4;
zipM lista3 lista4;

(* Resultado esperado *)
[
   [("p", false), ("q", false)],
   [("p", false), ("q", true) ],
   [("p", true),  ("q", false)],
   [("p", true),  ("q", true) ]
]

(* Resultado de zipL *)
(* Se obtiene este resultado erroneo debido a que la lista no se mantiene *)
(* Se logro usando zipL lista2 lista4; *)
[
   [("p", false), ("p", false)], 
   [("q", true),  ("q", false)]
]

(* Resultado de zipM *)
(* Se obtiene este resultado favorable debido a que se usa una lista de variables trucada *)
(* Se logro usando zipM lista3 lista4; *)
[
   [("p", false), ("q", false)], 
   [("p", true),  ("q", false)],
   [("p", false), ("q", true) ], 
   [("p", true),  ("q", true) ]
]

(* Resultado de as_val *)
(* Se obtiene este resultado favorable con todas las condiciones *)
(* Se logro usando as_val lista2 lista4; *)
[
   [("p", false), ("q", false)], 
   [("p", true),  ("q", false)],
   [("p", false), ("q", true) ], 
   [("p", true),  ("q", true) ]
]