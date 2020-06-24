(* Evaluador.

   Este es realmente el interprete.
   Hay un caso para cada variante dentro de las categorias sintacticas.
*)

(* Cuando se trata de invocar un identificador que no corresponde a 
   una función, se activa esta excepción *)

exception ErrorDeValor of string

(* evalExp evalua una expresion en un ambiente; se
   produce un valor semantico, en este caso una literal entera. *)

fun evalExp exp =
  case exp of
    ConstExp lit
       => Evaluado (lit)
  | OpMas (operando1,operando2)
       => let val (Evaluado valor1) = evalExp operando1
              and (Evaluado valor2) = evalExp operando2
          in  Evaluado (valor1 + valor2) (* suma *)
          end
  | OpMenos (operando1,operando2)
       => let val (Evaluado valor1) = evalExp operando1
              and (Evaluado valor2) = evalExp operando2
          in  Evaluado (valor1 - valor2) (* resta *)
          end
  | OpPor (operando1,operando2)
       => let val (Evaluado valor1) = evalExp operando1
              and (Evaluado valor2) = evalExp operando2
          in  Evaluado (valor1 * valor2) (* mult *)
          end
  | OpEntre (operando1,operando2)
       => let val (Evaluado valor1) = evalExp operando1
              and (Evaluado valor2) = evalExp operando2
          in  if valor2 = 0 then
                raise ErrorDeValor "division entre 0"
              else
                Evaluado (valor1 div valor2) (* div *)
          end
