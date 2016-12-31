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
-define(RELLENO, "      ").
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
	{Status, Salida} = file:open(ArchivoEntrada, [write]),
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
	if Largo =:= ?LARGO_LINEA -> io:fwrite(Salida,"~s\n", [ordenar_vector(Vector)]);
	   true -> io:format("error linea ~b, largo ~b debe ser ~b\n", [Nl, Largo, ?LARGO_LINEA]),
	   		   io:fwrite(Salida, "~s", [Vector])
	end.

ordenar_vector(Vector) ->
	{Encabezado,Periodos} = separar_y_ordenar_periodos(Vector),
	Largo = length(Periodos),
	if Largo =:= 0 -> Encabezado ++ string:left("N", ?TAM_RELLENO);
	   Largo > ?ELEMENTOS_VECTOR -> Encabezado ++ string:left("S", ?TAM_RELLENO);
	   true ->  P = lists:reverse(lists:sort(Periodos)),
	   			S = lists:flatten(P),
	   			L = (?TAM_RELLENO-length(S)) - 1,
	   			Encabezado ++ "D" ++ S ++ string:left(" ", L)
	end.

separar_y_ordenar_periodos(Vector) ->
	{string:left(Vector, ?POS_VECTOR), ordenar_periodos(separar_periodos(string:strip(Vector, right, $\n)))}.


separar_periodos(Vector) ->
	Largo = length(Vector),
	separar_periodos(string:substr(Vector, ?POS_VECTOR+1), [], Largo).


separar_periodos(Linea, Periodos, ?TAM_PERIODO) -> 
	if Linea =:= ?CEROS -> Periodos;
	   true -> [[Linea]|Periodos]
	end;

separar_periodos(Linea, Periodos, _) ->
	Periodo = string:substr(Linea, 1, ?TAM_PERIODO),
	Resto = string:substr(Linea, ?TAM_PERIODO+1),
	Largo = length(Resto),
	if Periodo =:= ?CEROS -> separar_periodos(Resto, Periodos, Largo);
	   true -> separar_periodos(Resto, [[Periodo]|Periodos], Largo)
	end.
	
ordenar_periodos(Periodos) ->
	Largo = length(Periodos),
	if Largo =:= 0 -> Periodos;
	   true ->
	S = sets:from_list(Periodos),
	sets:to_list(S)
end.


cronometrar(F) -> 
    statistics(wall_clock),
    F(),
    {_, RTime} = statistics(wall_clock),
    io:format("tiempo ocupado: ~.3f ~n", [RTime / 1000.0]).
