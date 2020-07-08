(* Distributiva *)

fun dis prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	negacion prop1
			=> negacion (dis prop1)
	|   conjuncion (disyuncion (prop1, prop2), disyuncion (prop3, prop4))
			=> disyuncion (dis prop1, conjuncion(dis prop2, dis prop4))
    |	disyuncion (conjuncion (prop1, prop2), conjuncion (prop3, prop4))
			=> conjuncion (dis prop1, disyuncion(dis prop2, dis prop4))
	|	implicacion (prop1, prop2)
			=> implicacion (dis prop1, dis prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (dis prop1, dis prop2)
;

(* ejemplo: dis ( (f :||: p) :&&: (f :||: q) ); *)
conjuncion(disyuncion(constante false, variable "p"), disyuncion(constante false, variable "q"))
(* ejemplo: dis ( (f :&&: p) :||: (f :&&: q) ); *)
disyuncion(conjuncion(constante false, variable "p"), conjuncion(constante false, variable "q"))