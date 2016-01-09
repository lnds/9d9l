# Toque y fama en Scala

Scala es el lenguaje de este grupo con el que tengo más experiencia.

Scala es un lenguaje muy amplio. Dominarlo completamente toma tiempo.

En este ejemplo tomé una solución distinta para calcular los toques y famas:

	def comparar(num:Array[Int], sec:Array[Int]) : (Int,Int) = {
		val tf = for ((n,i) <- num.zipWithIndex; (m,j) <- sec.zipWithIndex) 
					yield if (n == m) if (i == j) 'F' else  'T'
		(tf.filter(_ == 'T').length, tf.filter(_ == 'F').length)
	}

Construyo una lista de Ts y Fs, luego cuantos elementos hay de cada letra. Esto no es la forma más eficiente de resolver esto, pero permite destacar algunas características de Scala. Todo el resto del código es bastante "tradicional", salvo por el uso de Option (en otros lenguajes he usado nil o null para indicar un error).


## ¿Donde aprender Scala?

Yo estoy leyendo:

	- Programming in Scala, de Martin Odersky (autor del lenguaje), http://amzn.to/1S9Adly.
	- Scala for the Impatient, de Cay Hortsmann, http://amzn.to/1PODN17
