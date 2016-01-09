# Toque y fama en Haskell

Haskell tiene mala prensa. Todos piensan que es complicado, o muy académico.

Es un lenguaje exigente, pero si uno aprende lenguajes funcionales menos puros, como F#, Clojure y Erlang, puede aproximarse a Haskell con cierta confianza.

Personalmente me gusta programar en Haskell para mejorar mi estilo de programación.

Haskell es un lenguaje funcional puro, lo que hace que para implementar efectos laterales sea necesario usar Monadas, y es acá donde la mayoría de los que quieren aprender Haskell se confunde, asustan y desesperan.

Recomiendo tomarlo con calma, no preguntar qué son las Monadas y simplemente usarlas, el concepto se asienta con el tiempo.

Esta es la implementación más breve de este problema, así que vale la pena mirarla.

Decidí implementar una función que calcule toque y fama de una manera más similar al código en Erlang:

	toques_y_famas [] _ _ = (0,0)
	toques_y_famas (n:ns) (x:xs) ys = if n == x then (t,1+f) else (if n `elem` ys then (t+1, f) else (t,f))
  		where (t,f) = (toques_y_famas ns xs ys)

Es una versión recursiva, que sólo funciona cuando ambas listas tienen el mismo largo.

En general debo decir que esta solución es bastante "fea", así que es probable que retorne a ella en algún momento.

Me auto impuse un límite de tiempo para resolver los problemas, así que es la mejor solución que pude armar.

## ¿Donde aprender Haskell?

Yo estoy leyendo:

	- Learn You a Haskell for Great Good!, de Miran Lipovaca, en linea http://learnyouahaskell.com/
