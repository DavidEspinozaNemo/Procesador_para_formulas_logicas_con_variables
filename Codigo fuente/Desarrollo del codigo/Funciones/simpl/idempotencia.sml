(* fun idempotencia *)
(* David Espinoza *)

fun idempotencia prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	conjuncion ( prop1 , prop2 ) =>
			if (prop1 = prop2)
			then idempotencia prop1
			else conjuncion (idempotencia prop1, idempotencia prop2)
	|	disyuncion ( prop1 , prop2 ) =>
			if (prop1 = prop2)
			then idempotencia prop1
			else disyuncion (idempotencia prop1, idempotencia prop2)
	|	negacion prop1
			=> negacion (idempotencia prop1) 
	|	implicacion (prop1, prop2)
			=> implicacion (idempotencia prop1, idempotencia prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (idempotencia prop1, idempotencia prop2)
;

(* ejemplo: idempotencia ( f :=>: p :<=>: q :=>: p :&&: p ); *)