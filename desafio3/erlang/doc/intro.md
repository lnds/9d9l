# Ordenar Vectores en Erlang

Claramente Erlang no es un lenguaje diseñado para este tipo de labores, y sus bibliotecas estándar no tienen mucho soporte para esto.

La primera ejecución de este programa tomaba casi 4 minutos.

He logrado reducir el tiempo a cerca de 45 segundos, lo que considero suficiente.


## Archivos

Gran parte de la optimización se debe a que abrimos los archivos en modo raw. 
Notar la extraña forma de abrir los archivos:


    procesar_archivo(ArchivoEntrada, ArchivoSalida) ->
        {Status, Entrada} = file:open(ArchivoEntrada,[read, raw, read_ahead]),
        preparar_salida(Status,Entrada, ArchivoSalida).

    preparar_salida(error, Reason,_) ->
        io:format("ERROR, No pudo abrir archivo de entrada: ~s\n", [Reason]);

    preparar_salida(ok, Entrada, ArchivoEntrada) ->
        {Status, Salida} = file:open(ArchivoEntrada, [write, raw, delayed_write]),
        procesar_vectores(Status, Entrada, Salida, 0).


    procesar_vectores(error, _, Reason, _) ->
        io:format("ERROR, No pudo crear archivo de salida: ~s\n", [Reason]);

    procesar_vectores(ok, Entrada, Salida, Nl) ->
        ...


La función file:open retorna una tupla {Status, FileHandle}, si tiene error Status corresponde al atom error, si todo está bien, Status corresponde al atom ok.
Por eso que hacemos un patter matching de los argumentos para manejar estas situaciones de error.

Una vez que hemos abierto los dos archivos los procesamos del siguiente modo:

    procesar_vectores(ok, Entrada, Salida, Nl) ->
        case file:read_line(Entrada) of 
            eof -> file:close(Entrada), file:close(Salida);
            {ok,Linea} -> procesar_vector(Linea, Salida, Nl), procesar_vectores(ok, Entrada, Salida, Nl+1)
        end.

procesar_vectores es una función recursiva, en cada llamada leemos una linea del archivo de entrada, si esta función retorna el átomo eof entonces cerramos los archivos y salimos.
En caso contrario, hemos leido una linea y procesamos el vector contenido en esta, luego invocamos esta función incrementando el número de linea.


# Procesando strings

Primero verificamos el largo del vector de entrada, si corresponde lo procesamos, de lo contrario mostramos un error en la salida estándar y continuamos.

    procesar_vector(Vector, Salida, Nl) ->
        Largo = len(Vector),
        if Largo =:= ?LARGO_LINEA -> file:write(Salida, ordenar_vector(Vector)++"\n");
           true -> io:format("error linea ~b, largo ~b debe ser ~b\n", [Nl, Largo, ?LARGO_LINEA]),
                   file:write(Salida, Vector)
        end.

La función para ordenar vectores es como sigue:

    ordenar_vector(Vector) ->
        Encabezado = substr(Vector, 1, ?POS_VECTOR),
        Periodos = separar_periodos(substr(Vector, ?POS_VECTOR+1, ?LARGO_VECTOR), new(), ?LARGO_VECTOR),%separar_periodos(Vector),
        Largo = size(Periodos),
        if Largo =:= 0 -> [Encabezado|?N_RELLENO];
           Largo > ?ELEMENTOS_VECTOR -> [Encabezado|?S_RELLENO];
           true ->  P = reverse(to_list(Periodos)),
                    L = (?TAM_RELLENO-len(P)*?TAM_PERIODO) - 1,
                    [Encabezado,"D", P, left(" ", L)]
        end.

Para almacenar cada periodo usamos un ordset (conjunto ordenado) que creamos con la función new().
Al inicio del programa incluimos todos los siguientes módulos y funciones:

    -import (string, [substr/2, substr/3, len/1, strip/3, left/2]).
    -import (lists, [sort/2, flatten/1, reverse/1]).
    -import (ordsets, [to_list/1, add_element/2, size/1, new/0]).

La separación de los periodos se realiza del siguiente modo:

    separar_periodos(Linea, Periodos, ?TAM_PERIODO) -> 
        if Linea =:= ?CEROS -> Periodos;
           true -> add_element(Linea, Periodos)
        end;

    separar_periodos(Linea, Periodos, Largo) ->
        Periodo = substr(Linea, 1, ?TAM_PERIODO),
        Resto = substr(Linea, ?TAM_PERIODO+1),
        if Periodo =:= ?CEROS -> separar_periodos(Resto, Periodos, Largo-?TAM_PERIODO);
           true -> separar_periodos(Resto, add_element(Periodo, Periodos),  Largo-?TAM_PERIODO)
        end.

La primera versión ocurre cuando estamos procesando el último periodo que tendrá largo ?TAM_PERIODO.
Las demás llamadas serán las que irán acumulando los periodos dentro del vector en un conjunto llamado Periodos.

