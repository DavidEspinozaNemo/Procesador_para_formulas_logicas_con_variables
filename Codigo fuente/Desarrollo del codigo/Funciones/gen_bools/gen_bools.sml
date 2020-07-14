(* gen_bools opcional *)
(* David Espinoza *)

fun add [x] xs = [x] @ xs;

fun map f [] = []
| map f (x::xs) = f(x) :: map f xs;


(* - map (add [true] ) [[true, true], [false, false]];                    *)
(* > val it = [[true, true, true], [true, false, false]] : bool list list *)

fun gen_bools 1 = [[true, false]]
|   gen_bools 2 = [[true, true], [true, false], [false, true], [false, false]]
|   gen_bools n = (map (add [true] ) (gen_bools (n-1))) @ (map (add [false] ) (gen_bools (n-1)));