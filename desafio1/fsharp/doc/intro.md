# Toque y fama en F#

Hace mucho tiempo que no programo nada para .Net, pero ahora que tenemos Mono, F# es opensource y existe Visual Code, le daremos otra oportunidad a Microsoft.

Al igual que con Erlang, tuve que implementar Shuffle usando una implementación ineficiente, aunque creo que con F# es más fácil implementar Fisher-Yates que con Erlang (ese es un buen ejercicio para el lector).

Shuffle en F# queda así:

	let shuffle = List.sortBy(fun _ -> System.Guid.NewGuid())

Código tomado de StackOverflow y de este famoso post en Coding Horror: http://blog.codinghorror.com/shuffling/

F# es un lenguaje funcional no puro (lo que se agradece).

Me gusta mucho el operador |>, miren este código que permite seleccionar la secuencia aleatoria:

	let sec = [ 0 .. 9 ]  |> shuffle |> Seq.take tam |> List.ofSeq

Esto es similar al operador | (pipe) en Unix. Tenemos una lista formada por los números del 0 al 9 que se pasa como argumento a shuffle, de lo anterior se toman tam elementos (tam es 5) y la lista resultante se convierte en secuencia.

Las funciones se invocan como en Haskell, sin paréntesis, por ejemplo:

	jugar tam sec (i+1)

Esto invoca la función jugar con los parámetros tam, sec y (i+1).

El nucleo del juego lo implementé en la función jugar, que es recursiva.

Aunque en F# se puede construir un loop con while, preferí programar esta solución en un estilo más funcional inspirado por la solución en Erlang, aunque la solución en F# es más concisa.

F# es un lenguaje que respeta la indentación. Está prohibido usar tabs, aunque eso se puede relajar, a costa de usar ; y ;; (Argh!).

Por lo que he visto hasta ahora se puede decir que es un lenguaje elegante, vamos a ver como se desempeña más adelante.

## ¿Donde aprender F#?

Yo estoy leyendo:

	- The Book of F#, por Dave Fancher, http://amzn.to/1VRJqOm, pero en realidad está muy orientado para alguien que viene de .Net y C#.
	- El sitio de F# Foundation tiene varias referencias por explorar.

	

	

