(ns ordenar-vector.tools (:gen-class))

; show elapsed time of a function call
(defmacro mytime
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         msecs# (long (/ (- (. System (nanoTime)) start#) 1000000))
         secs#  (/ (rem (rem msecs# 3600000) 60000) 1000.0) ]  
     (println (str "tiempo ocupado: " (format "%02.3f" secs#)))
     ret#))