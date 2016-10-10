(ns ordenar-vector.core
  (:gen-class))

(use 'clojure.java.io
	 'ordenar-vector.tools)

;(require '[profile.core :refer :all])

(def ^:const pos-vector  9)
(def ^:const tam-periodo 6)
(def ^:const instituciones 6)
(def ^:const elementos  23) 
(def ^:const tam-vector (* 6 23))
(def ^:const tam-linea (+ pos-vector (* elementos  tam-periodo instituciones)))

(defn str-of [n c] (apply str (repeat n c)))

(def ^:const ceros (str-of tam-periodo \0))

(def ^:const relleno (str-of tam-periodo \space))
(def ^:const relleno-vector (str-of tam-vector \space))

(defn periodo-valido [p] (some #(not= \0 %) p))

(defn gt [x y] (pos? (compare x y)))

(defn agregar [v e]
	(loop [[y & ys] v acc []]
		(cond
			(= ceros e) v
			(nil? y) (conj acc e)
			(= e y) v
			(gt e y) (vec (concat acc [e y] ys))
			:else (recur ys (conj acc y)))))

(defn extraer-periodos [linea]
	(loop [periodos  linea lista []] 
		(let [periodo (subs periodos 0 tam-periodo)]
			(cond
				(= tam-periodo (count periodos)) (agregar lista periodo)
				(> (count lista) elementos) lista
				:else (recur (subs periodos tam-periodo) (agregar lista periodo))))))

(defn ordenar-periodos [linea]
	(let [p (extraer-periodos linea)
		  n (count p)]
		(if (= 0 n) 
			(cons "N" relleno-vector) 
			(if (> n elementos)
			 	(cons "S" relleno-vector)
			 	(cons "D" (take elementos (concat p (repeat relleno))))))))

(defn procesar-linea [linea]
	(let [encabezado (subs linea 0 pos-vector)
		  resto (ordenar-periodos (subs linea pos-vector))]
		  (apply str (cons encabezado resto))))

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

;(profile-vars procesar-vectores filtrar-linea procesar-linea ordenar-periodos  extraer-periodos relleno ceros str-of elementos tam-vector)

(defn -main
  "Implementa desafio 3, ordenar vectores"
  [& args]
  (mytime
  	;(profile {}
  	(if (== 2 (count args))
  	  (procesar-vectores (first args) (last args))
  	  (println "uso: ordenar-vector archivo_entrada archivo_salida"))))
;)
