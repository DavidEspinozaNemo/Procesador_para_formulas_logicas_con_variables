(* De Morgan *)
(* Anthony Ulloa - Jasson Gonzalez *)

fun de_morgan prop = 
	case prop of 
		constante valor
			=> constante valor
		|	variable valor
			=> variable valor
        |	negacion prop1
            => negacion (de_morgan prop1)
		|	conjuncion (negacion prop1, negacion prop2)
			=> negacion (disyuncion (de_morgan prop1, de_morgan prop2))
		|   disyuncion (negacion prop1, negacion prop2)
			=> negacion (conjuncion (de_morgan prop1, de_morgan prop2))
		|	implicacion (prop1, prop2)
			=> implicacion (de_morgan prop1, de_morgan prop2)
		|	equivalencia (prop1, prop2)
			=> equivalencia (de_morgan prop1, de_morgan prop2)
		|	conjuncion(prop1, prop2)
			=> negacion(disyuncion(negacion(de_morgan (prop1)), negacion(de_morgan (prop2))))
		|	disyuncion(prop1, prop2)
			=> negacion(conjuncion(negacion(de_morgan (prop1)), negacion(de_morgan (prop2))))
;


(* Version 2 Agregar validacion para negacion de conjuncion y negacion de disyuncion *)
fun de_morgan prop = 
	case prop of 
		constante valor
			=> constante valor
		|	variable valor
			=> variable valor
        |	negacion prop1
            => ( case prop1 of         
              	conjuncion(prop1, prop2) => disyuncion(negacion(de_morgan(prop1)), negacion (de_morgan(prop2)))
               | disyuncion(prop1, prop2) => conjuncion(negacion(de_morgan(prop1)), negacion (de_morgan(prop2)))
			   | _ => negacion (de_morgan prop1)
			   )
		|	conjuncion (negacion prop1, negacion prop2)
			=> negacion (disyuncion (de_morgan prop1, de_morgan prop2))
		|   disyuncion (negacion prop1, negacion prop2)
			=> negacion (conjuncion (de_morgan prop1, de_morgan prop2))
		|	implicacion (prop1, prop2)
			=> implicacion (de_morgan prop1, de_morgan prop2)
		|	equivalencia (prop1, prop2)
			=> equivalencia (de_morgan prop1, de_morgan prop2)
		|	conjuncion(prop1, prop2)
			=> negacion(disyuncion(negacion(de_morgan (prop1)), negacion(de_morgan (prop2))))
		|	disyuncion(prop1, prop2)
			=> negacion(conjuncion(negacion(de_morgan (prop1)), negacion(de_morgan (prop2))))
;


(* ejemplo: de_morgan( ~:t :||: ~:f ); *)

(* Falta agregar esto *)
(* ejemplo: de_morgan( ~:(f :&&: t) ); *)

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


