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
	(println "NEWS" (count news))
	(let [sorted-news (take 10 (sort-by #(> 0 (compare  (:pub %) (:pub %))) news))]
		(doseq [n sorted-news]
			(pr-news n))))

(defn -main
  "read an parse feeds"
  [& urls]
  	(print-sorted-news (flatten (pmap parse-news urls)))
  	(shutdown-agents))
  