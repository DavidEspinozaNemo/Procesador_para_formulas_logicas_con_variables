(* gen_bools opcional *)
(* Hecho por el profesor *)
(* No hace falta recibir algo como lista

fun add [x] xs = [x] @ xs;

*)

fun add x xs = x :: xs ;

(* map ya está pre-definida en Standard ML

fun map f []      = []
|   map f (x::xs) = f x :: map f xs;
*)


(* - map (add [true] ) [[true, true], [false, false]];                    *)
(* > val it = [[true, true, true], [true, false, false]] : bool list list *)

fun gen_bools 0 = [[]]
|   gen_bools n = map (add true) (gen_bools (n-1)) @ map (add false) (gen_bools (n-1));