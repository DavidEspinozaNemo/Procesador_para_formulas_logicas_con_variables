(* Distributiva *)
(* Anthony Ulloa *)

fun dis prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	negacion prop1
			=> negacion (dis prop1)
	|   conjuncion (disyuncion (prop1, prop2), disyuncion (prop3, prop4))
			=> if prop1 = prop3 then
			   		disyuncion (dis prop1, conjuncion(dis prop2, dis prop4))
			   else
			   		conjuncion (disyuncion (dis prop1, dis prop2), disyuncion (dis prop3, dis prop4))
    |	disyuncion (conjuncion (prop1, prop2), conjuncion (prop3, prop4))
			=> if prop1 = prop3 then
					conjuncion (dis prop1, disyuncion(dis prop2, dis prop4))
			   else
			   		disyuncion (conjuncion (dis prop1, dis prop2), conjuncion (dis prop3, dis prop4))
	|	implicacion (prop1, prop2)
			=> implicacion (dis prop1, dis prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (dis prop1, dis prop2)
	|	_
			=> prop
;

(* ejemplo: dis ( (f :||: p) :&&: (f :||: q) :=>: (f :&&: p) :||: (f :&&: q) ); *)
(* ejemplo: dis ( (f :&&: p) :||: (f :&&: q) ); *)