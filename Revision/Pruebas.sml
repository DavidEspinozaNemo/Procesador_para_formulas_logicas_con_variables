(* Pruebas de Funciones *)

(* vars *)
val p = variable "p";
val q = variable "q";
val r = variable "r";

val prop1 = (~:p :||: (~:q :=>: ~:p));
val prop2 = ((~:p :=>: ~:q):&&:(~:r));

vars(prop1);
(* ["p", "q"] *)

vars(prop1);
(* ["p", "q", "r"] *)

(* eval_prop *)
val p = variable "p";
val q = variable "q";

val prop1 = (~:(p :||: q));
val prop2 = ((~:(p :=>: q)) :&&: p);
val prop3 = ((p :||: q) :=>: q);
val prop4 = (q :=>: p);
val prop5 = p :&&: q :||: p;

val context = [("p", true), ("q", false)];

eval_prop(prop1, context);
(* false *)

eval_prop(prop2, context);
(* true *)

eval_prop(prop3, context);
(* false *)

eval_prop(prop4, context);
(* true *)

evalProp (prop5, [("p", true), ("q", true)]);
(* true *)

evalProp (prop5, [("p", false), ("q", true)]);
(* false *)

(* gen_bools *)
val p = variable "p";
val q = variable "q";

gen_bools(2);
(* [[true, true], [true, false], [false, true], [false, false]] *)

gen_bools(3);
(* [[true, true, true], [true, true, false], [true, false, true],
     [true, false, false], [false, true, true], [false, true, false],
     [false, false, true], [false, false, false]] *)

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

(* simpl *)
val p = variable "p";
val q = variable "q";
val r = variable "r";

val prop1 = (~:(p :||: q));
val prop2 = (~:p :||: (~:q :=>: ~:p));
val prop3 = ((~:p :&&: ~:q):||:(p:&&:r));
val prop4 = ((~:p :=>: ~:r):&&:(~:r));

simpl(prop1);
simpl(prop2);
simpl(prop3);
simpl(prop4);

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
val r = variable "r";

val prop1 = (~:p :&&: ~:q);
val prop2 = (~:(p :&&: q));
val prop3 = (~:(p :||: q));
val prop4 = (~:p :||: (~:q :=>: ~:p));
val prop5 = ((~:p :&&: ~:q) :&&: r);

de_morgan(prop1);
de_morgan(prop2);
de_morgan(prop3);
de_morgan(prop4);
de_morgan(prop5);

de_morgan( ~:(t :||: f ));

(* conmutativa *)
com ( f :||: p :<=>: q :&&: f );

(* asociativa *)
aso ( f :||: (q :||: p) :=>:  (f :&&: q) :&&: p );