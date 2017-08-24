(ns huffman.core

  (:gen-class))

(use 'huffman.io)

(defn usage [] (println "Uso: huffman [-c|-d] archivo_entrada archivo_salida"))

(defn leaf [symbol freq]
  (list :leaf freq symbol))

(defn leaf? [leaf]
  (= (first leaf) :leaf))

(defn weight [node]
  (nth node 1))

(defn node [left right]
  (list :node (+ (weight left) (weight right))  left right))

(defn node? [node]
  (= (first node) :node))

(defn sort-tree [tree]
  (sort-by  weight > tree))

(defn make-tree [leaves]
  (loop [trees leaves]
      (if (= 1 (count trees))
        (first trees)
        (recur (sort-tree (cons (node (first trees) (second trees)) (drop 2 trees))) ))))

(defn compress [input output]
  (let [bytes (read-bytes input)
        freqs (sort-by val > (seq (frequencies bytes)))
        leaves (map (partial apply leaf)  freqs)
        tree (make-tree leaves)]
    (println tree)))

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
