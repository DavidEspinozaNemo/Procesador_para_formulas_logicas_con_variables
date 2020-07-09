(* Conmutativa *)
(* Anthony Ulloa *)

fun com prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	negacion prop1
			=> negacion (com prop1)
	|	conjuncion (prop1, prop2)
			=> conjuncion (com prop2, com prop1)
	|   disyuncion (prop1, prop2)
			=> disyuncion (com prop2, com prop1)
	|	implicacion (prop1, prop2)
			=> implicacion (com prop1, com prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (com prop1, com prop2)
;

(* ejemplo: com ( f :||: p :<=>: q :&&: f ); *)