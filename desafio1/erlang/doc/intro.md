# Toque y fama en Erlang

Este es el primer programa que escribo en Erlang, así que quizás muchos programadores acostumbrados a este lenguaje le encuentren muchos defectos.

La estructura de control IF en Erlang es extraña (no existe else), lo que obliga a construcciones exóticas como esta:

	if Salir =:= true -> io:format("~ngracias por jugar, adios.~n");
	   Salir =:= false -> continuar(Accion, Tam, Sec, I)
    end.

Algunos programadores erlang habrían escrito:

	if Salir =:= true -> io:format("~ngracias por jugar, adios.~n");
	   true -> continuar(Accion, Tam, Sec, I)
   	end.

Lo que en mi opinión genera mucha más confusión, leí por ahí que es mejor evitar este estilo, así que voy a seguir ese consejo.

Se nota la influencia de Prolog en el pattern matching de Erlang, lo que lo hace un lenguaje divertido de aprender si has tenido experiencia con Prolog (la que por fortuna tengo).

Si no conoces Prolog, muchas cosas te van a parecer muy extrañas.

Veamos la forma de comparar y contar los toques y famas

	contar_toques_y_famas(Num,Sec) -> tyf(Num,Sec,Sec).

	tyf([],_,_) -> {0,0};
	tyf([HNS|TNS],[HXS|TXS],YS) -> 
	    {T,F} = tyf(TNS,TXS,YS),
	    if HNS =:= HXS -> {T,F+1};
	       HNS =/= HXS -> {T+toque(HNS,YS),F}
	    end.

	toque(X,XS) -> IN = lists:member(X,XS), if IN =:= true -> 1; IN =:= false -> 0 end.

contar_toques_y_famas(Num,Sec) es una función que cuenta los toques y famas comparando la secuencia ingresada por el jugador (Num) contra la secuencia generada por el programa (Sec).

Para hacer esto se apoya en tyf, pasándole la secuencia generada 2 veces.
¿Por qué? La respuesta está en que tyf() trata de contar al mismo tiempo toques y famas (contrastar con la versión en Clojure).
Primero busca las famas, comparano las cabeza de cada lista de numeros.

La lista Num se decompone en [HNS|TNS], donde HNS es la cabeza de la lista (primer elemento) y TNS la cola de la lista. Lo mismo con Sec que se descompone en [HXS|TXS].

Una copia de Sec se guarda en YS que se usa para contar los toques.

La función tyf es recursiva, si la lista Num es vacía retorna 0.
Sino, si HNS es igual a HXS entonces tenemos 1 fama y seguimos buscando en las respectivas colas. Al contrario, buscams si HNS pertenece a YS (que es una copia de Sec), usando la función toque. La función toque retorna 1 si HNS pertenece a YS. Esto permite incrementar la cantidad de toques. 

Esta implementación, a diferencia de la vista en Clojure, evita una resta y sólo ejecuta una pasada sobre las listas Num y Sec, a costa de tener que copiar la lista original en una variable auxiliar.

## Particularidades de Erlang

Creo que el flujo programa se puede seguir, considerando que la recursividad es indirecta. Puede haber una forma más limpia y eficiente de modelar el flujo, estoy seguro, eso queda propuesto para el lector.

Una cosa especial de Erlang es que no es posible llamar funciones en If o guards, es decir, no puedo escribir esto:

	if salir(Accion) =:= true -> io:format("~ngracias por jugar, adios.~n");
	   Salir =:= false -> continuar(Accion, Tam, Sec, I)
    end.

Al contrario, se debe hacer un pattern matching del siguiente modo:

	Salir = salir(Accion),
	if Salir =:= true -> io:format("~ngracias por jugar, adios.~n");
	   Salir =:= false -> continuar(Accion, Tam, Sec, I)
    end.

En Erlang las variables siempre empiezan con MAYUSCULAS. Los identificacores que empiezan en minúsculas son "átomos", así que eof, true, false y los nombres de las funciones son todos átomos.

### Generación de la secuencia aleatoria.

A diferencia de Clojure, no existe una función shuffle, la implementación que aparece acá fue encontrada en Stackoverflow, no es la más eficiente, consiste en asociar a la lista de números un dígito al azar y después ordenar por este dígito. Sospecho que implementar un Shuffle como Fisher-Yates (https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle) en Erlang debe ser complejo.


## ¿Donde aprender Clojure?

Yo estoy leyendo:

	- Programming Erlang, por Joe Armstrong (creador de Erlang), Programming Erlang, http://amzn.to/1Re7EDx
	- Learn You Some Erlang, por Fred Hebert, http://learnyousomeerlang.com/

	

	

