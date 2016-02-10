(ns crdr.core (:gen-class))

(require '[clojure.xml :as xml])

(defn format-date [date]
	(let [fmt (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
		(.format date fmt)))

(defn parse-date [fmt date]
	(java.time.LocalDateTime/parse date fmt))

(def parse-rfc1123  (partial parse-date  java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME))

(defn extract-tag-content [tag content]
	(let [elem (filter #(= tag (:tag %)) (:content content))]
		(first (:content (last elem)))))

(defn item-title [item] (extract-tag-content :title item))
(defn item-body  [item] (extract-tag-content :description item))
(defn item-pub   [item] 
	(let [date (parse-rfc1123 (extract-tag-content :pubDate item))]
		(format-date date)))

(defn item-data [item]
	{:title (item-title item), :pub (item-pub item), :body (item-body item)})

;(defn item-title [item] 
;	(let [title (filter #(= :title (:tag %)) (:content item))]
;		(first (:content (last title)))))

(defn parse-atom [content] (println "atom"))

(defn parse-rss  [content]
	(let [items (filter #(= :item (:tag %)) (:content (first content)))]
		(println (map item-data items))))

(defn parse-news [source] 
	(let [x (xml/parse source)]
		(condp = (:tag x)
			:feed (parse-atom (:content x))
			:rss  (parse-rss  (:content x))
			(str "unexpected format"))))

(defn -main
  "read an parse feeds"
  [url]
  (parse-news url))
