(* Aportado por Anthony Ulloa *)
(* filter filtra una lista de acuerdo con un predicado p *)

fun filter p []      = []
|   filter p (x::xs) = if p x then x :: filter p xs else filter p xs
;


(* nub obtiene una lista sin duplicados a partir de una lista arbitraria *)
fun nub []      = []
|   nub (x::xs) = x :: (nub (filter (fn y => x <> y) xs))
;

val pru1 = [1, 2, 3, 4, 1, 2, 3, 5, 1, 2, 6, 8, 6]
;

val pru2 = ["hola", "estoy", "celebrando", "un", "día", "hola", "un"]
;