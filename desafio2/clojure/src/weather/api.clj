(ns weather.api (:gen-class))


(require '[clojure.xml :as xml]
		 '[clojure.string :as str])

(def api-key (System/getenv "WEATHER_API_KEY"))


(defn api-url [city]
	(str "http://api.openweathermap.org/data/2.5/weather?q=" city "&mode=xml&units=metric&appid=" api-key "&lang=sp"))


(defn parse-xml [source]
	(try (xml/parse source)
		(catch org.xml.sax.SAXParseException e {:tag :error :content "xml format"})
		(catch java.io.IOException e {:tag :io-error})
		(catch Exception e {:tag :general-error :content e})))


(defn api-call [city n max-tries]
	(let [xml-result (parse-xml (api-url city))]
		(if (= (:tag xml-result) :io-error)
			(do (Thread/sleep 100) (api-call city (inc n) max-tries))
			xml-result)))

(defn extract-tag [tag x]
	(first (filter #(= tag (:tag %)) (:content x))))

(defn max-temp [x]
	(let [t (extract-tag :temperature x)]
		(double (read-string (:max (:attrs t))))))

(defn city [x]
	(let [c (extract-tag :city x)]
		(:name (:attrs c))))
	
(defn conditions [x]
	(let [w (extract-tag :weather x)]
		(:value (:attrs w))))

(def max-tries 10) ; when an error happens 

(defn weather-api [c]
 (let [x (api-call c 0 max-tries)]
 	(if (= (:tag x) :error)
 		{:city c :max-temp 0.0 :conditions "" :error (:content x)}
  		{:city (city x), :max-temp (max-temp x) :conditions (conditions x) :error nil})))