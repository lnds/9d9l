# crdr en Clojure

La paralelización de esta tarea en Clojure es ridiculamente trivial.
Primero escribí esta implementación:


	(print-sorted-news 
		(flatten (map parse-news urls))))

la función parse-news recibe una url y retorna una lista de estructuras con lo requerido (título, origen, fecha, texto). Entonces al hacer

	(map parse-news urls)

Obtehenemos una lista de listas, cada lista con las news de cada url.

Al hacer 

	(flatten (map parse-news urls))

Dejamos todas las news en una lista.

El resto es imprimirlas ordenadamente, que es lo que hace print-sorted-news, que se encarga de ordenar las noticias de forma descedente de acuerdo a la fecha de publicación:

	(defn print-sorted-news [news]
		(let [sorted-news (take max-news (sort-by :pub #(< 0 (compare %1 %2)) news))]
			(doseq [n sorted-news]
				(pr-news n))))

Si reemplazamos map, por pmap hacemos que la descarga de cada url se realice en paralelo:

	(pmap parse-news urls)

Y con eso paralelizamos la descarga de las urls!

# Parsing de XML y HTML.

Lo complicado fue hacer el parsing de XML. No está del todo correcto, así que lo revisaré más adelante.

Lo otro es que fue necesario usar un paquete externo para hacer parsing de HTML, hice esto porque no quería gastar mucho tiempo en ese aspecto. En este caso usé clj-tagsoup. 
