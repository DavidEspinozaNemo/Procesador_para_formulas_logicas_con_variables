(* Asociativa *)

fun aso prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	negacion prop1
			=> negacion (aso prop1)
    |	conjuncion (conjuncion (prop1, prop2), prop3)
			=> conjuncion (aso prop1, conjuncion (aso prop2, aso prop3))
	|	conjuncion (prop1, conjuncion (prop2, prop3))
			=> conjuncion (conjuncion (aso prop1, aso prop2), aso prop3)
    |   disyuncion (disyuncion (prop1, prop2), prop3)
            => disyuncion (aso prop1, disyuncion (aso prop2, aso prop3))
	|   disyuncion (prop1, disyuncion (prop2, prop3))
			=> disyuncion (disyuncion (aso prop1, aso prop2), aso prop3)
	|	implicacion (prop1, prop2)
			=> implicacion (aso prop1, aso prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (aso prop1, aso prop2)
;

(* ejemplo: aso ( f :||: (q :||: p) :=>:  (f :&&: q) :&&: p ); *)
