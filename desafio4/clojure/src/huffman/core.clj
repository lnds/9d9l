(ns huffman.core

  (:gen-class))

(use 'huffman.io)

(defn usage [] (println "Uso: huffman [-c|-d] archivo_entrada archivo_salida"))

(defn leaf [symbol freq] (list :leaf freq symbol))

(defn leaf? [leaf] (= (first leaf) :leaf))

(defn weight [node] (second node))

(defn node [left right] (list :node (+ (weight left) (weight right))  left right))

(defn node? [node] (= (first node) :node))

(defn sym [tree] (when-not (nil? tree) (when (leaf? tree) (nth tree 2))))

(defn left-node [tree] (when (node? tree) (nth tree 2)))

(defn right-node [tree] (when (node? tree) (nth tree 3)))

(defn sort-tree [tree] (sort-by  weight < tree))

(defn make-tree [leaves]
  (loop [trees leaves]
      (if (= 1 (count trees))
        (first trees)
        (recur (sort-tree (cons (node (first trees) (second trees)) (drop 2 trees))) ))))

(defn make-codes
  ([tree] (make-codes tree []))
  ([tree code]
    (if (leaf? tree)
      {(sym tree) code}
      (conj
        (make-codes (left-node tree) (conj code 0))
        (make-codes (right-node tree) (conj code 1))))))

(defn sym-as-bits [tree]
             (when (leaf? tree) (byte-to-bits (sym tree))))

(defn tree-as-bits [tree]
  (cond
    (leaf? tree) [1 (sym-as-bits tree)]
    (node? tree) [0 (flatten (tree-as-bits (left-node tree)))  (flatten (tree-as-bits (right-node tree)))]))

(defn- print-codes [codes]
  (doseq [key (sort (keys codes))]
    (println key "->" (get codes key))))

(defn compress [input output]
  (let [bytes (read-bytes input)
        freq (sort-by val < (seq (frequencies bytes)))
        leaves (map (partial apply leaf)  freq)
        tree (make-tree leaves)
        codes (make-codes tree)]
    (write-encoded output (flatten [(tree-as-bits tree) (int-to-bits (count bytes)) (flatten (map codes bytes))]))))

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
