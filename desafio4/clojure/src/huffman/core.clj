(ns huffman.core

  (:gen-class))

(use 'huffman.io)
(use 'huffman.bits)

(defn usage [] (println "Uso: huffman [-c|-d] archivo_entrada archivo_salida"))

(defn leaf [symbol freq] (list :leaf freq symbol))

(defn leaf? [leaf] (= (first leaf) :leaf))

(defn weight [node] (second node))

(defn node [left right] (list :node (+ (weight left) (weight right))  left right))

(defn node? [node] (= (first node) :node))

(defn sym [tree]  (nth tree 2))

(defn left-node [tree] (nth tree 2))

(defn right-node [tree] (nth tree 3))

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

(defn sym-as-bits [tree]  (byte-to-bits (sym tree)))

(defn tree-as-bits [tree]
  (cond
    (leaf? tree) [1 (sym-as-bits tree)]
    (node? tree) [0 (tree-as-bits (left-node tree))  (tree-as-bits (right-node tree))]))

(defn encode-bits [codes bytes]
  (flatten (map codes bytes)))

(defn compress [input output]
  (let [bytes (read-bytes input)
        freq (sort-by val <  (frequencies bytes))
        leaves (map (partial apply leaf) freq)
        tree (make-tree leaves)
        codes (make-codes tree)]
    (write-encoded output (flatten [(tree-as-bits tree)
                                    (encode-bits codes bytes)] ))))


(declare read-tree)

(defn read-sym [bits]
          [(drop 8 bits) (bits-to-byte (take 8 bits))])

(defn read-leaf [bits]
  (let [[rest-bits sym] (read-sym bits)]
    [rest-bits (leaf sym -1)]))

(defn read-node [bits]
  (let [[right-bits left] (read-tree bits)
        [rest-bits right] (read-tree right-bits)]
    [rest-bits (node left right)]))

(defn read-tree [bits]
  (if (= 1 (first bits))
    (read-leaf (rest bits))
    (read-node (rest bits))))

(defn read-int [bits]
  (bits-to-int (take 32 bits)))

(declare tree-decode)

(defn leaf-decode [tree bits]
  [bits (sym tree)])

(defn node-decode [tree bits]
  (if (= 1 (first bits))
    (tree-decode (right-node tree) (rest bits))
    (tree-decode (left-node tree) (rest bits))))

(defn tree-decode [tree bits]
  (if (leaf? tree)
    (leaf-decode tree bits)
    (node-decode tree bits)))

(defn decode-bits [tree coded-bits]
  (loop [bits coded-bits bytes (transient [])]
    (if (empty? bits)
      (persistent! bytes)
      (let [[rest-bits sym] (tree-decode tree bits)]
        (recur rest-bits (conj! bytes sym))))))

(defn decompress [input output]
  (let [bytes (read-bytes input)
        bits (flatten (encode-bytes-to-bits bytes))
        [rest-bits tree] (read-tree bits)
        codes (make-codes tree)
        out-bytes (decode-bits tree rest-bits)]
    (write-bytes output (byte-array out-bytes))))

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
