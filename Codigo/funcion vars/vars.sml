(* Responsable: David Espinoza *)

(* Funcion vars *)
(* 1ro para que funcione necesito al tipo Proposicion *)
datatype Proposicion =
   constante   of bool
|  variable    of string
|  negacion    of Proposicion
|  conjuncion  of Proposicion * Proposicion
|  disyuncion  of Proposicion * Proposicion
|  implicacion of Proposicion * Proposicion
| equivalencia of Proposicion * Proposicion
;

nonfix ~:
val ~: = negacion

infix 7 :&&:
val (op :&&:) = conjuncion

infix 6 :||:
val (op :||:) = disyuncion

infix 5 :=>:
val (op :=>:) = implicacion

infix 4 :<=>:
val (op :<=>:) = equivalencia

;
(* El ejemplo de entrada *)
val p = variable "p";
val q = variable "q";
val f = constante false;
val t = constante true;
val prop1 = p :=>: q :<=>: ~: p :||: q;
val prop2 = f :=>: p :<=>: q :=>: ~: f;
(* vars *)
type str = string;

fun vars prop =
	case prop of
		constante valor
			=> [ ]
	|	variable valor
			=> [valor]
	|	negacion prop1
			=> vars prop1
	|	conjuncion (prop1, prop2)
			=> let val valor1 = vars prop1
				   and valor2 = vars prop2
			   in valor1 @ valor2
			   end
	|   disyuncion (prop1, prop2)
			=> let val valor1 = vars prop1
				   and valor2 = vars prop2
			   in valor1 @ valor2
			   end
	|	implicacion (prop1, prop2)
			=> let val valor1 = vars prop1
				   and valor2 = vars prop2
			   in valor1 @ valor2
			   end
	|	equivalencia (prop1, prop2)
			=> let val valor1 = vars prop1
				   and valor2 = vars prop2
			   in valor1 @ valor2
			   end
;