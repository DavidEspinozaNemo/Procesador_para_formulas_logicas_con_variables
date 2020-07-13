fun simp prop = 
    aso(com(de_morgan(dis(doble_neg(idempotencia(neutro(prop)))))))
;

(* sugerencia simp v2 *)
(* David Espinoza *)

(* funcion contar elementos de de una proposicion *)

fun cont_elem_prop prop = 
	case prop of
		constante valor
			=> 1
	|	variable valor
			=> 1
	|	conjuncion (prop1, prop2) 
			=> 1 + cont_elem_prop prop1 + cont_elem_prop prop2
	|   disyuncion (prop1, prop2)
			=> 1 + cont_elem_prop prop1 + cont_elem_prop prop2
	|	negacion prop1
			=> 1 + cont_elem_prop prop1
	|	implicacion (prop1, prop2)
			=> 1 + cont_elem_prop prop1 + cont_elem_prop prop2
	|	equivalencia (prop1, prop2)
			=> 1 + cont_elem_prop prop1 + cont_elem_prop prop2
;

fun simpv2 prop 5 = simp prop
fun simpv2 prop n = 
	if ( cont_elem_prop (simp prop) ) < ( cont_elem_prop prop )
	then simp prop
	else simpv2 (simp prop) (n+1)
;