(* De Morgan *)
(* No sirve todavia *)
fun de_morgan prop = 
	case prop of 
		constante valor
			=> constante valor
		|	variable valor
			=> variable valor
        |	negacion prop1
            => negacion (de_morgan prop1)
		|	negacion (prop1, prop2)
			=> de_morgan (de_morgan prop1, de_morgan prop2)
		|	conjuncion (prop1, prop2)
			=> disyuncion (de_morgan prop1, de_morgan prop2)
		|   disyuncion (prop1, prop2)
			=> conjuncion (de_morgan prop1, de_morgan prop2)
		|	implicacion (prop1, prop2)
			=> implicacion (de_morgan prop1, de_morgan prop2)
		|	equivalencia (prop1, prop2)
			=> equivalencia (de_morgan prop1, de_morgan prop2)
;

(* ejemplo: de_morgan( ~:(f :&&: t) :=>: ~:t :||: ~:f ); *)