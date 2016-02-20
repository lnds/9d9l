(ns crdr.core (:gen-class))


(use 'crdr.news)

(defn pr-news [n]
	(println "Título: " (:title n))
	(println "Fuente: " (:source n))
	(println "Fecha: " (:pub n))
	(doseq [line (take 3 (:body n))]
		(println (subs line 0 (min (count line) 140))))
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
     (prn (str "Tiempo ocupado en descargar las noticias: "  (format "%d:%02d:%02.3f" hours# mins# secs#)))
     ret#))

(defn -main
  "read an parse feeds"
  [& urls]
  (mytime
  	(do (print-sorted-news (flatten (pmap parse-news urls)))
  		(shutdown-agents)))) ;; shutdown-agents prevents a 1 minutes wait after pmap
  