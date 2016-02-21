(ns crdr.core (:gen-class))


(use 'crdr.news)

(def line-size 140)

(defn print-error [n]
	(println "Fuente: " (:source n))
	(println "Error:  " (:error n)))

(defn print-ok [n]
	(println "Título: " (:title n))
	(println "Fuente: " (:source n))
	(println "Fecha: " (:pub n))
	(doseq [line (take 3 (:body n))]
		(println (subs line 0 (min (count line) line-size)))))

(defn pr-news [n]
	(if (:error n) (print-error n) (print-ok n))
	(println))

(defn print-sorted-news [news]
	(let [sorted-news (take 10 (sort-by :pub #(< 0 (compare %1 %2)) news))]
		(doseq [n sorted-news]
			(pr-news n))))

(defmacro mytime
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         msecs# (long (/ (- (. System (nanoTime)) start#) 1000000))
         hours# (quot msecs# 3600000)
         mins#  (quot (rem msecs# 3600000) 60000)
         secs#  (/ (rem (rem msecs# 3600000) 60000) 1000.0) ]  
     (println (str "Tiempo ocupado en descargar las noticias: "  (format "%d:%02d:%02.3f" hours# mins# secs#)))
     ret#))

(defn -main
  "read an parse feeds"
  [& urls]
  	(do (mytime (print-sorted-news (flatten (remove nil? (pmap parse-news urls)))))
  		(shutdown-agents))) ;; shutdown-agents prevents a 1 minutes wait after pmap
  