type Literal = int

(* Este es un lenguaje de expresiones, con las siguientes opciones:
   - Literal (entera)
   - Operaciones aritméticas entre enteros
*)

datatype Expresion =
           ConstExp   of Literal
         | OpMas      of Expresion * Expresion
         | OpMenos    of Expresion * Expresion
         | OpPor      of Expresion * Expresion
         | OpEntre    of Expresion * Expresion
