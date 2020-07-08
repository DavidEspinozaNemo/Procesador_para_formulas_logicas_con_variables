(* fun neutro *)
(* David Espinoza *)

(* necesito una función que me determine si una prop es variable o no. *)
fun isVariable prop =
	case prop of
		constante valor
			=> false
	|	variable valor
			=> true
	|	conjuncion ( prop1 , prop2 ) 
			=> false
	|	disyuncion ( prop1 , prop2 ) 
			=> false	
	|	negacion prop1
			=> false
	|	implicacion (prop1, prop2)
			=> false
	|	equivalencia (prop1, prop2)
			=> false
;

fun neutro prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	conjuncion (prop1, prop2) =>
			if (prop2 = constante true) then if (isVariable prop1) then neutro prop1
			else if (prop1 = constante true) then if (isVariable prop2) then neutro prop2
			else conjuncion (neutro prop1, neutro prop2)
	|	disyuncion (prop1, prop2) =>
			if (prop2 = constante false) then if (isVariable prop1) then neutro prop1
			else if (prop1 = constante false) then if (isVariable prop2) then neutro prop2
			else disyuncion (neutro prop1, neutro prop2)
	|	negacion prop1
			=> negacion (neutro prop1) 
	|	implicacion (prop1, prop2)
			=> implicacion (neutro prop1, neutro prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (neutro prop1, neutro prop2)
;

(* ejemplo: neutro ( f :=>: p :<=>: q :=>: p :||: f ); *)