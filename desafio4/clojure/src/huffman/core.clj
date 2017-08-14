(ns huffman.core
  (:gen-class))

(defn usage [] (println "Uso: huffman [-c|-d] archivo_entrada archivo_salida"))

(defn compress [input output])

(defn decompress [input output])

(defn process [[cmd input-file output-file]]
  (case cmd
    "-c" (compress input-file output-file)
    "-d" (decompress input-file output-file)
    (usage)))

(defn -main
  "Huffman encoding decoding"
  [& args]
  (if (or (empty? args) (not= 3 (count args)))
    (usage)
    (process args)))
