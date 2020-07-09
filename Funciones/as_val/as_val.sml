(* ANTHONY ULLOA - 2018290801 *)
(* Funcion AS_VAL *)
(* Defina una función, as_vals que, dada una lista de variables proposicionales sin repeticiones, la combina con
una lista de valores booleanos (true o false) de la misma longitud, para producir una lista del tipo
(string * bool) list que combina, posicionalmente, cada variable proposicional con el
correspondiente valor booleano. Esto lo denominamos una asignación de valores (a las variables
proposicionales). *)

(* Entradas *)

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