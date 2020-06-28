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

//Prueba as_val
let lista_booleanos = [[false,false],[true,false],[false,true],[true,true]];

let asignacion_valores = as_val(["p", "q"], lista_booleanos, []);

console.log("Estas son las asignaciones para p y q con sus posibles valores: ", asignacion_valores);