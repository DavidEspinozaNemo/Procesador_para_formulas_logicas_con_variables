(* fun neutro *)
(* David Espinoza *)

(* necesito una función que me determine si una prop es variable o no. *)
fun isVariable prop =
	case prop of	
		variable _  => true
	|	         _  => false
;

fun neutro prop = 
	case prop of
		constante valor
			=> constante valor
	|	variable valor
			=> variable valor
	|	conjuncion (prop1, constante true) 
			=> neutro prop1
	|	conjuncion (constante true, prop2) 
			=> neutro prop2
	|	conjuncion (prop1, prop2) 
			=> conjuncion (neutro prop1, neutro prop2)
	|	disyuncion (prop1, prop2) =>
			if (prop2 = constante false) then neutro prop1
			else if (prop1 = constante false) then neutro prop2
			else disyuncion (neutro prop1, neutro prop2)
	|	negacion prop1
			=> negacion (neutro prop1) 
	|	implicacion (constante true, prop2)
			=> neutro prop2
	|	implicacion (constante false, _)
			=> constante true
	|	implicacion (prop1, prop2)
			=> implicacion (neutro prop1, neutro prop2)
	|	equivalencia (prop1, prop2)
			=> equivalencia (neutro prop1, neutro prop2)
;

(* ejemplo: neutro ( f :=>: p :<=>: q :=>: p :||: f ); *)