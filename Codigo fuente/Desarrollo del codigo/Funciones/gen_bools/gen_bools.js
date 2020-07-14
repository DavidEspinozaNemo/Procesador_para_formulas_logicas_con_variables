//Generar arreglos de booleanos a partir de una lista de valores proposicionales
const gen_bools = (lista_variables_proposicionales, combinaciones)=>{
    //total_combinaciones representa el numero de posibles combinaciones 2**n
    let total_combinaciones = 2**lista_variables_proposicionales.length;

    //Contador que esta siendo analizado para generar su equivalente a 1s y 0s
    let numero_a_convertir = combinaciones.length;

    //Caso base. Si el numero de combinaciones actuales es igual al numero de combinaciones posibles retornar las combinaciones
    if(combinaciones.length == total_combinaciones){
      return combinaciones;
    }
    else {
        //Cantidad de 0s y 1s que van a agregarse en la combinacion 
        let cantidad_variables_proposicionales = lista_variables_proposicionales.length;

         //Arreglo de valores booleanos que sera insertada en la lista de combinaciones
         let nuevaCombinacion = get_bool_combination(cantidad_variables_proposicionales, numero_a_convertir, []);;
        
        //lista_variables_proposicionales
        combinaciones.push(nuevaCombinacion);
        return gen_bools(lista_variables_proposicionales, combinaciones); 
    }
}

//numero a convertir es un contador que permite luego convertir dicho numero a una lista de 1s y 0s
//combinacion es el arreglo que almacena booleanos [true, false, ...]

const get_bool_combination = (cantidad_variables_proposicionales, numero_a_convertir, combinacion)=> {
    //Revisa si se han procesado los valores para todas las variables proposicionales
    if (cantidad_variables_proposicionales == 0){
        return combinacion;
    }

    //Mientras existan elementos proposicionales
    else {
        //Obtener un valor booleano a partir de un contador decimal
        if(numero_a_convertir%2 < 1){
            combinacion.push(false);
        }
        else{
            combinacion.push(true);
        }
        //Descomponer el numero actual para extraer todos los 1s y 0s y en base a la cantidad de variables proposicionales
        numero_a_convertir = numero_a_convertir/2;
        cantidad_variables_proposicionales = cantidad_variables_proposicionales-1;

        return get_bool_combination(cantidad_variables_proposicionales, numero_a_convertir, combinacion);
    }
}

const as_val = (lista_variables_proposicionales, lista_valores_booleanos, asignaciones_finales) => {
    if(asignaciones_finales.length == lista_valores_booleanos.length){
        return asignaciones_finales;
    }
    else{
        let curr_as = as_val_aux(lista_variables_proposicionales, lista_valores_booleanos[asignaciones_finales.length], []);
        asignaciones_finales.push(curr_as);
        return as_val(lista_variables_proposicionales, lista_valores_booleanos, asignaciones_finales);
    }
}

const as_val_aux = (lista_variables_proposicionales, lista_valores_booleanos, asignaciones) => {
    //Caso base. Para todos los valores proposicionales se ha generado una asociacion con un valor booleano
    if(asignaciones.length == lista_variables_proposicionales.length){
        return asignaciones;
    }
    else {
        //Indice de elementos a relacionar
        let indice = asignaciones.length;

        //Generacion de tupla
        let tupla_prop_bool = [lista_variables_proposicionales[indice], lista_valores_booleanos[indice]];

        //Insercion de tupla en lista de asignaciones de todos los elementos proposicionales
        asignaciones.push(tupla_prop_bool);

        //Llamada recursiva
        return as_val_aux(lista_variables_proposicionales, lista_valores_booleanos, asignaciones);
    }
}


//Prueba gen_bools
let lista_booleanos = gen_bools(["p", "q", "r", "s"], []);

//Prueba as_val
let asignacion_valores = as_val(["p", "q", "r", "s"], lista_booleanos, []);

console.log("Estos son los posibles valores booleanos para p y q: ", lista_booleanos);
console.log("Estas son las asignaciones para p y q con sus posibles valores: ", asignacion_valores);