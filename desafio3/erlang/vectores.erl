-module (vectores).
-export ([main/0, main/1]).

-define(ERROR, "uso: ordenar_vectores archivo_entrada archivo_salida\n").
-define(POS_VECTOR, 9).
-define(ELEMENTOS_VECTOR, 23).
-define(TAM_PERIODO, 6).
-define(CANT_INSTITUCIONES,  6).
-define(TAM_VECTOR,  138).
-define(TAM_VECTOR_ENTRADA, 828).
-define(LARGO_LINEA, 838).
-define(TAM_SALIDA, 148).
-define(TAM_RELLENO, 139).
-define(S_RELLENO, "S                                                                                                                                          ").
-define(N_RELLENO, "N                                                                                                                                          ").
-define(CEROS, "000000").


main() -> io:format(?ERROR).
main([Entrada,Salida]) -> 
	cronometrar(fun() -> 
		procesar_archivo(Entrada, Salida)
	end).


procesar_archivo(ArchivoEntrada, ArchivoSalida) ->
	{Status, Entrada} = file:open(ArchivoEntrada,[read, raw, read_ahead]),
	preparar_salida(Status,Entrada, ArchivoSalida).

preparar_salida(error, Reason,_) ->
	io:format("ERROR, No pudo abrir archivo de entrada: ~s\n", [Reason]);

preparar_salida(ok, Entrada, ArchivoEntrada) ->
	{Status, Salida} = file:open(ArchivoEntrada, [write, delayed_write]),
	procesar_vectores(Status, Entrada, Salida, 0).


procesar_vectores(error, _, Reason, _) ->
	io:format("ERROR, No pudo crear archivo de salida: ~s\n", [Reason]);

procesar_vectores(ok, Entrada, Salida, Nl) ->
	case file:read_line(Entrada) of 
		eof -> file:close(Entrada);
		{ok,Linea} -> procesar_vector(Linea, Salida, Nl), procesar_vectores(ok, Entrada, Salida, Nl+1)
	end.

procesar_vector(Vector, Salida, Nl) ->
	Largo = string:len(Vector),
	if Largo =:= ?LARGO_LINEA -> file:write(Salida, ordenar_vector(Vector)++"\n");
	   true -> io:format("error linea ~b, largo ~b debe ser ~b\n", [Nl, Largo, ?LARGO_LINEA]),
	   		   file:write(Salida, Vector)
	end.

ordenar_vector(Vector) ->
	Encabezado = string:left(Vector, ?POS_VECTOR),
	Periodos = sets:to_list(separar_periodos(string:strip(Vector, right, $\n))),
	Largo = length(Periodos),
	if Largo =:= 0 -> [Encabezado|?N_RELLENO];
	   Largo > ?ELEMENTOS_VECTOR -> [Encabezado|?S_RELLENO];
	   true ->  P = lists:sort(fun(X,Y) -> Y < X end, Periodos),
	   			S = lists:flatten(P),
	   			L = (?TAM_RELLENO-length(S)) - 1,
	   			[Encabezado,"D",S,string:left(" ", L)]
	end.

separar_periodos(Vector) ->
	Largo = length(Vector),
	separar_periodos(string:substr(Vector, ?POS_VECTOR+1), sets:new(), Largo-?POS_VECTOR).


separar_periodos(Linea, Periodos, ?TAM_PERIODO) -> 
	if Linea =:= ?CEROS -> Periodos;
	   true -> sets:add_element(Linea, Periodos)
	end;

separar_periodos(Linea, Periodos, Largo) ->
	Periodo = string:substr(Linea, 1, ?TAM_PERIODO),
	Resto = string:substr(Linea, ?TAM_PERIODO+1),
	if Periodo =:= ?CEROS -> separar_periodos(Resto, Periodos, Largo-?TAM_PERIODO);
	   true -> separar_periodos(Resto, sets:add_element(Periodo, Periodos),  Largo-?TAM_PERIODO)
	end.
	

cronometrar(F) -> 
    statistics(wall_clock),
    F(),
    {_, RTime} = statistics(wall_clock),
    io:format("tiempo ocupado: ~.3f ~n", [RTime / 1000.0]).
