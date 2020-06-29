(* ANTHONY ULLOA - 2018290801 *)
(* Funcion AS_VAL *)
(* Defina una función, as_vals que, dada una lista de variables proposicionales sin repeticiones, la combina con
una lista de valores booleanos (true o false) de la misma longitud, para producir una lista del tipo
(string * bool) list que combina, posicionalmente, cada variable proposicional con el
correspondiente valor booleano. Esto lo denominamos una asignación de valores (a las variables
proposicionales). *)

(* Entrada: ["p"], ["p","q"], ["p","q","r"] *)

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

val lista0  = [["p","q"],["p","q"],["p","q"],["p","q"]];

val lista1  = ["p"];
val lista2  = [[true], [false]];

val lista3  = ["p","q"];
val lista4  = [[true, true], [true, false], [false, true], [false, false]];
              
val lista5  = ["p","q","r"];
val lista6  = [[true, true, true], [true, true, false], [true, false, true], [true, false, false], [false, true, true], [false, true, false], [false, false, true], [false, false, false]];
               
val lista7  = ["p","q","r","s"];
val lista8  = [[true, true, true, true], [true, true, true, false],
     [true, true, false, true], [true, true, false, false],
     [true, false, true, true], [true, false, true, false],
     [true, false, false, true], [true, false, false, false],
     [false, true, true, true], [false, true, true, false],
     [false, true, false, true], [false, true, false, false],
     [false, false, true, true], [false, false, true, false],
     [false, false, false, true], [false, false, false, false]];

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

******************************************************
(* Resultado de as_val *)
(* Se obtiene este resultado favorable para N *)
(* Se logro usando la funcion add, as_val_aux y as_val *)
(* Se debe de ingresar primero la lista de variables y 
luego la lista de expresiones booleanas*)

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