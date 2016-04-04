(ns weather.core (:gen-class))

(use 'weather.api)

(def no-cities-provided-message "debe ingresar una lista de ciudades")
(def time-report-message "tiempo ocupado para generar el reporte: ")
(def report-format "%-30.30s max:%5.1f  min:%5.1f   actual: %5.1f %s")

(defn print-weather [reports]
  (if (empty? reports)
    (println no-cities-provided-message)
    (doseq [report reports]
      (if (:error report)
        (println (:city report) "Error:" (:error report))
        (println (format report-format (:city report) (:max report) (:min report) (:temp report) (:conditions report))))))
    (shutdown-agents))

(defn sort-reports [reports]
  (sort-by :temp #(< 0 (compare %1 %2)) reports))

(defn weather-report [cities]
  (let [p (first cities) r (rest cities)]
    (if (= "-p" p) 
      (pmap weather-api r)
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
     (println (str time-report-message (format "%d:%02d:%02.3f" hours# mins# secs#)))
     ret#))

(defn -main
  "read weather report"
  [& cities]
    (if (empty? cities)
      (println no-cities-provided-message)
      (mytime (print-weather (sort-reports (weather-report cities)))))) 