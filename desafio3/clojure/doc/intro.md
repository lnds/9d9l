# ordenar_vector en Clojure

## Archivos

El manejo de archivos es relativamente sencillo en Clojure, gracias a la macro (with-open), podemos abrir readers y writes


	(defn procesar-vectores [entrada salida]
		(if (not (.exists (file entrada))) 
			(println (str "no pudo abrir archivo" entrada))
		;; else
			(with-open [rdr (reader (file entrada) :buffer-size 4096)]
				(with-open [wrt (writer (file salida))]
					(doseq [linea (map-indexed vector (line-seq rdr))]
						(let [v (filtrar-linea linea)]
							(doto wrt (.write v) (.newLine))))))))

Notar como usamos (map-indexed vector (line-seq rdr)), esto permite leer secuencialmente linea a linea el archivo e indexar cada linea con un número, que es usado para desplegar error en la función filtrar-linea:

	(defn filtrar-linea [par-linea-n]
		(let [[n ^String linea] par-linea-n]
			(if (= tam-linea (.length linea)) 
				(.concat (subs linea 0 pos-vector) (ordenar-periodos linea))
			;; else
				(do (println (str "error en linea " n))
					linea))))

## Slices y Strings

En la solución Clojure tratamos cada linea como strings, no como arreglos de bytes como en C. Esto tiene la ventaja de que el programa en Clojure es más interoperable que otras soluciones e igual de eficiente.

Ordenar los periodos se hace de este modo

	(defn ordenar-periodos [^String linea]
		(let [periodos  (extraer-periodos linea)
			  n (count periodos)]
			 (cond 
				(zero? n) (.concat "N" relleno-vector)
			 	(> n elementos) (.concat "S" relleno-vector)
			 	:else (.concat "D" (s/join (take elementos (concat (sort #(compare ^String %2 ^String %1) periodos) (repeat relleno))))))))


Notar como le damos un hint al compilador, indicando que linea es String, esto permite optimizar las llamadas a funciones de Strings provistas por la JVM.

La función extraer-periodos es la siguiente

	; lista debe ser un set
	(defn extraer-periodos [^String linea]
		(let [len (- (.length linea) tam-periodo)]
			(loop [ini pos-vector fin (+ ini tam-periodo) lista (transient #{})]
				(if (= ini len) (persistent! (agregar-periodo linea ini fin lista))
				(recur (+ ini tam-periodo) (+ fin tam-periodo) (agregar-periodo  linea ini fin lista))))))

Es un loop que recorre el string de entrada (linea) comparando cada segmento e insertándolo en un Set. Esto permite eliminar duplicados.

La función agregar-periodo es la siguiente:

	(defn agregar-periodo [^String linea ini fin lista]
		(if (.regionMatches linea ini ceros 0 tam-periodo)
			lista 
			(conj! lista (subs linea ini fin))))

Esta función usa un método de los Strings en Java (.regionMatches) que permite comparar contra un segmento de string. 
Las primeras soluciones en Clojure creaban muchos substring, con esto se redujo significativamente el desempeño.


# Propuesto

¿Se puede optimizar más este código?