# Toque y Fama

El desafío consiste en implementar el juego Toque y Fama.

En este juego dos jugadores (A y B) eligen cada uno una cifra de 5 números del 0 al 9.
En cada turno un jugador (por ejemplo A), trata de adivinar la cifra de su oponente (en este ejemplo, el jugador B).
El jugador B compara la cifra propuesta por A con la que él ha elegido. 
Cada vez que B acierte un dígito en la posición correcta se la considera como una "Fama", cada vez que acierte a una dígito, pero en la posición incorrecta se cuenta como un Toque.

Por ejemplo:

B pensó en la cifra "23790".
A propone "25971".

En este caso el 2 está en la posición correcta por lo que corresponde a "1 Fama".

Los números 9 y 7 pertenecen a la cifra de B, pero están en posiciones distintas, en este caso tenemos "2 Toques".

Luego, B le responde al jugador A con la frase: "2 Toques y 1 Fama". 
El jugador A toma nota de esto y debe intentar deducir la cifra de B en base a esta información.

A continuación le toca el turno al jugador B para preguntar, y se sigue la misma dinámica descrita.

Gana aquel que primero llegue a las 5 Famas (es decir, acierte a la cifra).

## El desafío

En este desafío debemos construir una versión más simple de este juego. 

En este caso el programa elegirá una cifra al azar y es el usuario el que debe tratar de adivinarla.

La interacción se debe hacer a través de la consola.

Al iniciar el programa se debe desplegar las reglas del juego.

El programa debe generar una secuencia de 5 dígitos, los que no deben repetirse.

El usuario debe ingresar una cifra de 5 dígitos no deben repetirse. 

Si el usuario ingresa la palabra salir el juego termina.

Si el usuario ingresa EOF (^D o Control-D en Linux y Mac) el juego termina (es equivalente a escribir salir).

Si el usuario ingresa un cifra errónea (con digitos repetidos) o cualquier cosa que no sea un número o la palabra 
salir debe responde con el mensaje "error!".

El programa debe comparar la entrada del usuario con la secuencia generada aleatoriamente y debe reportar la cantidad de Toques y la cantidad de Famas que obtuvo.

Si el usuario obtiene 5 Famas, el programa debe avisar que ganó e imprimir un mensaje.

A continuación se muestra una sesión del juego:

	Bienvenido a Toque y Fama.
	==========================

	En este juego debes tratar de adivinar una secuencia de 5 dígitos generadas por el programa.
	Para esto ingresas 5 dígitos distintos con el fin de adivinar la secuencia.
	Si has adivinado correctamente la posición de un dígito se produce una Fama.
	Si has adivinado uno de los dígitos de la secuencia, pero en una posición distinta se trata de un Toque.

	Ejemplo: Si la secuencia es secuencia: [8, 0, 6, 1, 3] e ingresas 40863, entonces en pantalla aparecerá:
	tu ingresaste [4, 0, 8, 6, 3]
	resultado: 2 Toques 2 Famas


	Ingresa una secuencia de 5 dígitos distintos (o escribe salir):
	12345
	tu ingresaste [1, 2, 3, 4, 5]
	resultado: 1 Toques 1 Famas

	Ingresa una secuencia de 5 dígitos distintos (o escribe salir):
	67890
	tu ingresaste [6, 7, 8, 9, 0]
	resultado: 3 Toques 0 Famas

	Ingresa una secuencia de 5 dígitos distintos (o escribe salir):
	67915
	tu ingresaste [6, 7, 9, 1, 5]
	resultado: 4 Toques 1 Famas

	Ingresa una secuencia de 5 dígitos distintos (o escribe salir):
	97615
	tu ingresaste [9, 7, 6, 1, 5]
	resultado: 3 Toques 2 Famas

	Ingresa una secuencia de 5 dígitos distintos (o escribe salir):
	91756
	tu ingresaste [9, 1, 7, 5, 6]
	resultado: 2 Toques 3 Famas

	Ingresa una secuencia de 5 dígitos distintos (o escribe salir):
	91765
	tu ingresaste [9, 1, 7, 6, 5]
	resultado: 0 Toques 5 Famas

	Ganaste! Acertaste al intento 6! La secuencia era [9, 1, 7, 6, 5].

## Las implementaciones
	
	Este juego ha sido implementado en los lenguajes definidos en el proyecto "9 desafíos en 9 lenguajes": Clojure,Erlang, F#, Go, Haskell, Kotlin, Rust, Scala y Swift.

	Los detalles están descritos en cada archivo README para cada implementación.

# Resultados

## Lineas de código

	Calculadas usando la herramienta cloc

	| Clojure | 49 |
	| Erlang  | 61 |
	| F#      | 52 |
	| Go      | 85 |
	| Haskell | 41 |
	| Kotlin  | 77 |
	| Rust    | 82 |
	| Scala   | 55 |

# Licencia

	(c) 2016 Eduardo Díaz.

	El código de este proyecto se distribuye bajo licencia MIT, ver el archivo LICENSE para los detalles.


