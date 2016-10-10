(ns ordenar-vector.core
  (:gen-class))

(use 'clojure.java.io
	 'ordenar-vector.tools)

(def ^:const pos-vector  9)
(def ^:const tam-periodo 6)
(def ^:const instituciones 6)
(def ^:const elementos  23) 
(def ^:const tam-vector (+ (* elementos tam-periodo)  pos-vector  1))
(def ^:const tam-linea (+ pos-vector (* elementos  tam-periodo instituciones)))

(defn str-of [n c] (apply str (repeat n c)))

(def ^:const ceros (str-of tam-periodo \0))

(def ^:const relleno (str-of tam-periodo \space))

(defn in? [coll elm]  (some (partial = elm) coll))

(defn agregar-periodo [periodo lista]
	(if (or (= ceros periodo) (in? lista periodo)) 
		lista 
		(cons periodo lista)))

(defn extraer-periodos [linea]
	(loop [periodos linea lista []]
		(let [periodo (subs periodos 0 tam-periodo)]
			(if (= tam-periodo (count periodos))
				(agregar-periodo periodo lista)
		        (recur (subs periodos tam-periodo) (agregar-periodo periodo lista))))))

(defn clasificar-resultado [periodos]
	(let [n (count periodos)]
		(if (zero? n) 
			(str "N" (apply str (repeat elementos relleno)))
		 (if (> n elementos)
		 	(str "S" (apply str (repeat elementos relleno)))
		 	(str "D" (apply str (take elementos (concat (reverse (take elementos (sort periodos))) (repeat relleno)))))))))

(defn ordenar-periodos [linea]
	(let [periodos (extraer-periodos linea)]
		 (clasificar-resultado periodos)))

(defn procesar-linea [linea]
	(let [encabezado (subs linea 0 pos-vector)
		  resto (ordenar-periodos (subs linea pos-vector))]
		  (str encabezado resto)))

(defn filtrar-linea [par-linea-n]
	(let [[n linea] par-linea-n]
		(if (== tam-linea (count linea))  
			(procesar-linea linea)
		;; else
			(do (println (str "error en linea " n))
				linea))))

(defn procesar-vectores [entrada salida]
	(if (not (.exists (file entrada))) 
		(println (str "no pudo abrir archivo" entrada))
	;; else
		(with-open [rdr (reader (file entrada))]
			(with-open [wrt (writer (file salida))]
				(doseq [linea (map-indexed vector (line-seq rdr))]
					(let [v (filtrar-linea linea)]
						(doto wrt (.write v) (.newLine))))))))


(defn -main
  "Implementa desafio 3, ordenar vectores"
  [& args]
  (mytime 
  	(if (== 2 (count args))
  	  (procesar-vectores (first args) (last args)) 
  	  (println "uso: ordenar-vector archivo_entrada archivo_salida"))))
