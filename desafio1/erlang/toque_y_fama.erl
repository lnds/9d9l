%% -*- coding: utf-8 -*-
-module (toque_y_fama).
-export ([main/0]).


main() -> Tam = 5, mostrar_reglas(Tam), 
		  Sec = lists:sublist(generar_secuencia(), Tam),  
		  jugar(Tam,Sec,1).

jugar(Tam,Sec,I) -> 
    Accion = ingresar(Tam), 
    Salir = salir(Accion),
	if Salir =:= true -> io:format("~ngracias por jugar, adios.~n");
	   Salir =:= false -> continuar(Accion, Tam, Sec, I)
    end.

mostrar_reglas(Tam) -> io:format("Bienvenido a Toque y Fama.\n"++
		"==========================\n\n"++
		"En este juego debes tratar de adivinar una secuencia de ~w dígitos generadas por el programa.\n"++
		"Para esto ingresas ~w dígitos distintos con el fin de adivinar la secuencia.\n"++
		"Si has adivinado correctamente la posición de un dígito se produce una Fama.\n"++
		"Si has adivinado uno de los dígitos de la secuencia, pero en una posición distinta se trata de un Toque.\n\n"++
		"Ejemplo: Si la secuencia es secuencia: [8, 0, 6, 1, 3] e ingresas 40863, entonces en pantalla aparecerá:\n"++
		"tu ingresaste [4, 0, 8, 6, 3]\n"++
		"resultado: 2 Toques 2 Famas\n\n\n", [Tam, Tam]).

generar_secuencia() ->
	L = lists:seq(0,9), random:seed(erlang:system_time()),
    [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- L])].

ingresar(Tam) -> 
	io:format("Ingresa una secuencia de ~B digitos distintos (o escribe salir):~n", [Tam]),
	io:get_line("").

salir(eof) -> true;
salir("salir\n") -> true;
salir(_) -> false.

continuar(Accion, Tam, Sec, I) -> Num = validar(Accion, Tam),
	if Num =:= [] -> io:format("error!\n"), jugar(Tam,Sec,I+1);
       Num =/= [] -> revisar_jugada(Num,Sec,Tam, I)
    end.

revisar_jugada(Num, Sec, Tam, I) -> 
	io:format("ingresaste: ~w~n", [Num]),
	{Toques,Famas} = contar_toques_y_famas(Num,Sec),
	io:format("~w Toques, ~w Famas~n~n", [Toques,Famas]),
	if Famas =:= Tam -> io:format("Ganaste! Acertaste al intento ~w! La secuencia era ~w.~n", [I, Sec]);
	   Famas =/= Tam -> jugar(Tam, Sec, I+1)
	end.

contar_toques_y_famas(Num,Sec) -> tyf(Num,Sec,Sec).

tyf([],_,_) -> {0,0};
tyf([HNS|TNS],[HXS|TXS],YS) -> 
    {T,F} = tyf(TNS,TXS,YS),
    if HNS =:= HXS -> {T,F+1};
       HNS =/= HXS -> {T+toque(HNS,YS),F}
    end.

toque(X,XS) -> IN = lists:member(X,XS), if IN =:= true -> 1; IN =:= false -> 0 end.

to_num(L) -> to_num(L, []).

to_num([], L) -> lists:reverse(L);
to_num([H|T], L) -> to_num(T, [(H-$0)|L]).

is_digit(C) when C >= $0, C=<$9 -> true;
is_digit(_) -> false.

unicos(Num, Tam, Len) -> L = length(lists:usort(Num)), 
    if L =:= Len, L =:= Tam -> Num;
       L =/= Len; L =/= Tam -> [] end.

validar(Accion, Tam) -> Entrada = string:strip(Accion, both, $\n),
    Len = length(Entrada),
	Digitos = lists:all(fun (X) -> is_digit(X) end, Entrada), 
	if Digitos =:= true -> unicos(to_num(Entrada), Tam, Len) ; 
	   Digitos =:= false -> [] end.





