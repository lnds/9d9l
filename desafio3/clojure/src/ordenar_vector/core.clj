(ns ordenar-vector.core
  (:gen-class))

(use 'clojure.java.io
	 'ordenar-vector.tools)

(def ^:const pos-vector  9)
(def ^:const tam-periodo 6)
(def ^:const instituciones 6)
(def ^:const elementos  23) 
(def ^:const tam-linea 837) ; (+ pos-vector (* elementos  tam-periodo instituciones)))

(def ^:const ^String ceros "000000") ; (str-of tam-periodo \0))

(def ^:const ^String relleno "      ") ; (str-of tam-periodo \space))
(def ^:const ^String relleno-vector (clojure.string/join (repeat elementos relleno)))

(defn agregar-periodo [^String periodo lista]
	(if (= ceros periodo)
		lista 
		(conj! lista periodo)))

; lista debe ser un set
(defn extraer-periodos [^String linea]
	(let [len (.length linea)]
		(loop [ini pos-vector fin (+ ini tam-periodo) lista (transient #{})]
			(if (= ini len) (persistent! lista)
			(recur (+ ini tam-periodo) (+ fin tam-periodo) (agregar-periodo (.substring linea ini fin) lista))))))


(defn ordenar-periodos [^String linea]
	(let [periodos  (extraer-periodos linea)
		  n (count periodos)]
		 (cond 
			(zero? n) (str "N" relleno-vector)
		 	(> n elementos) (str "S" relleno-vector)
		 	:else (str "D" (clojure.string/join (take elementos (concat (sort #(compare %2 %1) periodos) (repeat relleno))))))))

(defn filtrar-linea [par-linea-n]
	(let [[n ^String linea] par-linea-n]
		(if (= tam-linea (.length linea)) 
			(str (.substring linea 0 pos-vector) (ordenar-periodos linea))
		;; else
			(do (println (str "error en linea " n))
				linea))))

(defn procesar-vectores [entrada salida]
	(if (not (.exists (file entrada))) 
		(println (str "no pudo abrir archivo" entrada))
	;; else
		(with-open [rdr (reader (file entrada) :buffer-size 4096)]
			(with-open [wrt (writer (file salida))]
				(doseq [linea (map-indexed vector (line-seq rdr))]
					(let [v (filtrar-linea linea)]
						(doto wrt (.write v) (.newLine))))))))

(defn -main
  "Implementa desafio 3, ordenar vectores"
  [& args]
  (mytime 
  	;(profile {}
  	(if (== 2 (count args))
  	  (procesar-vectores (first args) (last args)) 
  	  (println "uso: ordenar-vector archivo_entrada archivo_salida"))))
