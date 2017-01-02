(ns ordenar-vector.core
  (:gen-class)
  (:require [clojure.string :as s]))

(use 'clojure.java.io
	 'ordenar-vector.tools)

(def ^:const pos-vector  9)
(def ^:const pos-segundo-periodo 15)
(def ^:const tam-periodo 6)
(def ^:const instituciones 6)
(def ^:const elementos  23) 
(def ^:const tam-linea 837) ; (+ pos-vector (* elementos  tam-periodo instituciones)))
(def ^:const tope-linea 831)

(def ^:const ^String ceros  "000000") ; (str-of tam-periodo \0))
(def ^:const ^String relleno  "      ") ; (str-of tam-periodo \space))
(def ^:const ^String relleno-vector (s/join (repeat elementos relleno)))

(defn agregar-periodo [^String linea ini fin lista]
	(if (.regionMatches linea ini ceros 0 tam-periodo)
		lista 
		(conj! lista (subs linea ini fin))))

; lista debe ser un set
(defn extraer-periodos [^String linea]
	(loop [ini pos-vector fin pos-segundo-periodo lista (transient #{})]
		(if (= ini tope-linea) (persistent! (agregar-periodo linea ini fin lista))
		(recur (+ ini tam-periodo) (+ fin tam-periodo) (agregar-periodo  linea ini fin lista)))))


(defn ordenar-periodos [^String linea]
	(let [periodos  (extraer-periodos linea)
		  n (count periodos)]
		 (cond 
			(zero? n) (.concat "N" relleno-vector)
		 	(> n elementos) (.concat "S" relleno-vector)
		 	:else (.concat "D" (s/join (take elementos (concat (sort #(compare ^String %2 ^String %1) periodos) (repeat relleno))))))))

(defn filtrar-linea [[n ^String linea]]
	(if (= tam-linea (.length linea)) 
		(str (subs linea 0 pos-vector) (ordenar-periodos linea) "\n")
	;; else
		(do (println (str "error en linea " n))
			(str linea "\n"))))

(defn procesar-vectores [entrada salida]
	(if-not (.exists (file entrada)) 
		(println "no pudo abrir archivo" entrada)
	;; else
		(with-open [rdr (reader (file entrada) :buffer-size 4096)]
			(with-open [wrt (writer (file salida))]
				(doseq [linea (map-indexed vector (line-seq rdr))]
					(.write wrt (filtrar-linea linea)))))))

(defn -main
  "Implementa desafio 3, ordenar vectores"
  [& args]
  (mytime 
  	;(profile {}
  	(if (== 2 (count args))
  	  (procesar-vectores (first args) (last args)) 
  	  (println "uso: ordenar-vector archivo_entrada archivo_salida"))))
