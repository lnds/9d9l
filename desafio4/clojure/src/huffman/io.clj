(ns huffman.io
  (:import (java.nio.file Paths Files OpenOption Path)))

(defn get-path [filename]
  (Paths/get filename (make-array String 0)))


(defn read-bytes [filename]
  (Files/readAllBytes (get-path filename)))

(defn- pad-bits-to-byte-size [bits pad-size]
  (let [l (count bits)]
    (if (< l pad-size)
      (flatten (cons bits (repeat (- pad-size l) 0)))
      bits)))

(defn ^Byte bits-to-byte [b]
    (loop [byte 0 bits  (pad-bits-to-byte-size b 8) ]
            (if (empty? bits)
              byte
              (recur (bit-or (bit-shift-left byte 1) (first bits)) (rest bits)))))

(defn byte-to-bits [b]
  (loop [byt (short b) bits []]
    (if (zero? byt)
      (vec (reverse (pad-bits-to-byte-size bits 8)) )
      (recur (short (unsigned-bit-shift-right byt 1)) (conj bits (short (bit-and byt 0x01)))))))

(defn int-to-bits [i]
  (loop [byt (int i) bits []]
    (if (zero? byt)
      (vec (reverse (pad-bits-to-byte-size bits 32)) )
      (recur (int (unsigned-bit-shift-right byt 1)) (conj bits (int (bit-and byt 0x01)))))))

(defn encode-bits-to-bytes [bits]
  (map bits-to-byte (partition 8 bits)))

(defn write-bytes [^String filename ^"[B" bytes]
  (Files/write ^Path (get-path filename) ^"[B" bytes ^"[Ljava.nio.file.OpenOption;" (make-array OpenOption 0)))

(defn write-encoded [filename  bits]
  (let [bytes (encode-bits-to-bytes bits)]
    (println (count bits))
    (println (count bytes))
    (write-bytes filename (byte-array bytes))))