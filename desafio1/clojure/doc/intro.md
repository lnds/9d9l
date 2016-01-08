# Toque y fama en Clojure

Hay mucha gente que odia la sintáxis (o falta de la misma) de LISP. 

Consideren el siguiente truco (que se lo escuché a Uncle Bob Martin)

	Si tienen en un lenguaje cualquiera la llamada a una función de la siguiente forma:

	    F (X)

	Simplemente corran el paréntsis izquierdo de este modo

	   (F X)


	Por otro lado, si tienen esto:

		F(G(x,y))

	En Clojure se expresa así:

		(F (G x, y)) 

	Pero también así

		(F (G x y))

	Porque en Clojure las comas son equivalentes a espacios en blanco.

Otro consejo para escribir en Lisp o en Clojure: escriban funciones con pocas lineas de código.


## Notas sobre la implementación

Esta solución divide el problema de determinar cuantos toques y famas en 2 funciones:

	(defn famas [num sec] 
  		(count (filter #(= (get % 0) (get % 1)) (map vector num sec))))

	(defn toques [num sec]
  		(count (filter #(contains? (set sec) %) num)))

Hay que notar que toques contará además de los toques las famas, es por esto que debemos restar el valor de famas para obtener la cuenta correcta:

	(defn contar-toques-famas [num sec]
	  (let [t (toques num sec) f (famas num sec)]
	    [(- t f) f]))

La función validar aprovecha el hecho de que en Clojure nil es falso y una secuencia puede ser considerada como verdadero, de este modo, esta función aparte de validar el string de entrada devuelve la entrada convertida en una lista de enteros.

Esta no es la solución más breve ni la más óptima, optimizar el código es un buen ejercicio para el lector.

La idea de esta implementación es mostrar la idea de construir programas siguiendo el método bottom-up propio de Lisp. Además tratamos de usar características propias de Clojure, como (loop), llamadas a funciones anónimas o lambdas y funciones para operar sobre listas (ver las funciones solo-sigitos, validar, toques y famas).

## ¿Donde aprender Clojure?

Yo estoy leyendo:

  - Programming Clojure, de Stuart Halloway, http://amzn.to/1VQk6bv
  - Clojure Applied, de Ben Vandgrift y Alex Miller, http://amzn.to/1PT6qMs

