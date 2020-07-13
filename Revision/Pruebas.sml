(* Pruebas de Funciones *)

(* vars *)

(* eval_prop *)

(* gen_bools *)
val p = variable "p";
val q = variable "q";

val prop1 = p :&&: q :||: p;
evalProp (prop1, [("p", true), ("q", true)]);
evalProp (prop1, [("p", false), ("q", true)]);

(* as_vals *)
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

as_vals lista1 lista2;
as_vals lista3 lista4;
as_vals lista5 lista6;
as_vals lista7 lista8;

(* taut *)

(* simp *)

(* neutro *)
neutro ( f :=>: p :<=>: q :=>: p :||: f );

(* idempotencia *)
idempotencia ( f :=>: p :<=>: q :=>: p :&&: p );

(* doble_negacion *)
doble_neg ( f :=>: p :<=>: q :=>: ~:( ~:( ~: ( ~: f))) );

(* distributiva *)
dis ( (f :||: p) :&&: (f :||: q) :=>: (f :&&: p) :||: (f :&&: q) );
dis ( (f :&&: p) :||: (f :&&: q) );

(* de_morgan *)
val p = variable "p";
val q = variable "q";

val prop1 = (~:p :&&: ~:q);
val prop2 = (~:(p :&&: q));
val prop3 = (~:(p :||: q));
val prop4 = (~:p :||: (~:q :=>: ~:p));

de_morgan(prop1);
de_morgan(prop2);
de_morgan(prop3);
de_morgan(prop4);

de_morgan( ~:(t :||: f ));

(* conmutativa *)
com ( f :||: p :<=>: q :&&: f );

(* asociativa *)
aso ( f :||: (q :||: p) :=>:  (f :&&: q) :&&: p );