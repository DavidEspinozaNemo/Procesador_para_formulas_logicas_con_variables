(* De Morgan *)
(* Anthony Ulloa *)

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
;
(* ejemplo: de_morgan( ~:t :||: ~:f ); *)

(* Falta agregar esto *)
(* ejemplo: de_morgan( ~:(f :&&: t) ); *)

		|	negacion (conjuncion (prop1, prop2))
			=> disyuncion (de_morgan negacion(prop1), de_morgan negacion(prop2))

		|   disyuncion (negacion prop1, negacion prop2)
			=> negacion (conjuncion (de_morgan prop1, de_morgan prop2))