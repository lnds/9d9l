(ns crdr.news (:gen-class))


(require '[clojure.xml :as xml]
		 '[clojure.string :as str]
         '[pl.danieljanus.tagsoup :as ts])

;;;; dates ;;;;

; .atStartOfDay es necesario para convertir LocalDate a LocalDateTime
; ver http://stackoverflow.com/questions/27454025/unable-to-obtain-localdatetime-from-temporalaccessor-when-parsing-localdatetime?answertab=active#tab-top
(defn parse-date [fmt date]
	(.atStartOfDay (java.time.LocalDate/parse date fmt)))

(def parse-rfc1123  (partial parse-date  java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME))

(def parse-rfc3339  (partial parse-date  java.time.format.DateTimeFormatter/ISO_OFFSET_DATE_TIME))

(defn format-date [date]
	(let [fmt (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")]
		(.format date fmt)))

;;;; html ;;;;

(defn filter-html [elements]
	(filter #(or (and (keyword? %) (= :p %)) (string? %)) (flatten elements)))

(defn p-to-nl [body]
	 (map #(if (= :p %) "\n" %) body))

(defn parse-html [body]
	(let [h  (filter-html (ts/parse-string body))]
			(str/split-lines (apply str (p-to-nl (if (= :p (first h)) (rest h) h))))))

;;;; xml ;;;;

(defn extract-tag-content [tag content]
	(let [elem (filter #(= tag (:tag %)) (:content content))]
		(first (:content (last elem)))))

(defn extract-title [item] (extract-tag-content :title item))

(defn item-body  [item] (extract-tag-content :description item))
(defn item-pub   [item] 
	(let [date (parse-rfc1123 (extract-tag-content :pubDate item))]
		(format-date date)))

(defn entry-body  [entry] 
	(let [content (extract-tag-content :content entry)]
		(if (nil? content)
			(extract-tag-content :summary entry)
			content))) 

(defn entry-pub   [entry] 
	(let [date (parse-rfc3339 (extract-tag-content :updated entry))]
		(format-date date)))

(defn item-data [url item]
	{:title (extract-title item), :source url, :pub (item-pub item), :body (parse-html (item-body item))})

(defn entry-data [url entry]
	(let [entry-content (:content entry)]
	{:title (extract-title entry), :source url,  :pub (entry-pub entry), :body (parse-html (entry-body entry))}))

(defn parse-atom [url content] 
	(let [entries (filter #(= :entry (:tag %)) content)]
		(map #(entry-data url %) entries)))

(defn parse-rss  [url content]
	(let [items (filter #(= :item (:tag %)) (:content (first content)))]
		(map #(item-data url %)  items)))

(defn parse-news [source] 
	(let [x (xml/parse source)]
		(condp = (:tag x)
			:feed (parse-atom source (:content x))
			:rss  (parse-rss  source (:content x)))))
