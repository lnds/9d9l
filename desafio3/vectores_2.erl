-module (vectores).
-compile({no_auto_import,[size/1]}).
-export ([main/0, main/1]).
-import (string, [substr/2, substr/3, len/1, strip/3, left/2, chars/2]).
-import (lists, [sort/2, flatten/1, reverse/1]).
-import (ordsets, [to_list/1, add_element/2, size/1, new/0]).

-define(ERROR, "uso: ordenar_vectores archivo_entrada archivo_salida\n").
-define(POS_VECTOR, 9).
-define(INI_VECTOR, 10).
-define(ELEMENTOS_VECTOR, 23).
-define(TAM_PERIODO, 6).
-define(TAM_PERIODO_MAS_1, 7).
-define(CANT_INSTITUCIONES,  6).
-define(TAM_VECTOR,  138).
-define(TAM_VECTOR_ENTRADA, 828).
-define(LARGO_LINEA, 838).
-define(TAM_SALIDA, 148).
-define(TAM_RELLENO, 139).
-define(LARGO_VECTOR, 828).
-define(S_RELLENO, "S                                                                                                                                          ").
-define(N_RELLENO, "N                                                                                                                                          ").
-define(CEROS, "000000").


main() -> io:format(?ERROR).
main([Entrada,Salida]) -> 
	cronometrar(fun() -> 
		procesar_archivo(Entrada, Salida)
	end);
main([_,_|_]) -> io:format(?ERROR).


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
	case file:read_line(Entrada) of 
		eof -> file:close(Entrada), file:close(Salida);
		{ok,Linea} -> procesar_vector(Linea, Salida, Nl), procesar_vectores(ok, Entrada, Salida, Nl+1)
	end.

procesar_vector(Vector, Salida, Nl) ->
	Largo = len(Vector),
	if Largo =:= ?LARGO_LINEA -> 
			file:write(Salida, [ordenar_vector(Vector)|"\n"]);
	   true -> io:format("error linea ~b, largo ~b debe ser ~b\n", [Nl, Largo, ?LARGO_LINEA]),
	   		   file:write(Salida, Vector)
	end.

ordenar_vector(Vector) ->
	Encabezado = substr(Vector, 1, ?POS_VECTOR),
	Periodos = separar_periodos(substr(Vector, ?INI_VECTOR, ?LARGO_VECTOR), new(), ?LARGO_VECTOR),
	Largo = size(Periodos),
	if Largo =:= 0 -> [Encabezado|?N_RELLENO];
	   Largo > ?ELEMENTOS_VECTOR -> [Encabezado|?S_RELLENO];
	   true ->  P = reverse(to_list(Periodos)),
	   			L = (?TAM_RELLENO-len(P)*?TAM_PERIODO) - 1, 
	   			[Encabezado, "D", P, chars(32, L)]
	end.

separar_periodos(Linea, Periodos, ?TAM_PERIODO) -> 
	if Linea =:= ?CEROS -> Periodos;
	   true -> add_element(Linea, Periodos)
	end;

separar_periodos(Linea, Periodos, Largo) ->
	Periodo = substr(Linea, 1, ?TAM_PERIODO),
	Resto = substr(Linea, ?TAM_PERIODO_MAS_1),
	if Periodo =:= ?CEROS -> separar_periodos(Resto, Periodos, Largo-?TAM_PERIODO);
	   true -> separar_periodos(Resto, add_element(Periodo, Periodos),  Largo-?TAM_PERIODO)
	end.
	

cronometrar(F) -> 
    statistics(wall_clock),
    F(),
    {_, RTime} = statistics(wall_clock),
    io:format("tiempo ocupado: ~.3f ~n", [RTime / 1000.0]).
