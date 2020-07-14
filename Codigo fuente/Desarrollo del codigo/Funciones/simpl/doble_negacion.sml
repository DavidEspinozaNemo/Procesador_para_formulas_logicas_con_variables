(* Doble Negación *)

fun doble_neg prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	negacion( negacion( prop1))
			=> doble_neg prop1
	|	negacion prop1
			=> negacion (doble_neg prop1)
	|	conjuncion (prop1, prop2)
			=> conjuncion (doble_neg prop1, doble_neg prop2)
	|   disyuncion (prop1, prop2)
			=> disyuncion (doble_neg prop1, doble_neg prop2)
	|	implicacion (prop1, prop2)
			=> implicacion (doble_neg prop1, doble_neg prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (doble_neg prop1, doble_neg prop2)
;

(* ejemplo: doble_neg ( f :=>: p :<=>: q :=>: ~:( ~:( ~: ( ~: f))) ); *)