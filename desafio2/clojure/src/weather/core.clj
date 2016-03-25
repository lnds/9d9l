(ns weather.core (:gen-class))

(def no-cities-provided-message "debe ingresar una lista de ciudades")
(def report-format "%-30.30s %02.2f %s")

(defn print-weather [reports]
  (doseq [report reports]
    (println (format report-format (:city report) (:max-temp report) (:conditions report))))
  (shutdown-agents))

(defn weather-api [city]
  {:city city, :max-temp 25.0 :conditions "Despejado"})

(defn weather-report [cities]
  (let [p (first cities)]
    (if (= "-p" p) 
      (pmap weather-api (rest cities))
      (map weather-api cities))))

; show elapsed time of a function call
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
  "read weather report"
  [& cities]
    (if (empty? cities)
      (println no-cities-provided-message)
      (mytime (print-weather (weather-report cities)))) ) 