(ns huffman.bits)

(defn- pad-bits-to-byte-size [bits pad-size]
  (let [l (count bits)]
    (if (< l pad-size)
      (flatten (cons bits (repeat (- pad-size l) 0)))
      bits)))

(defn- ^Short bits-to-byte! [b]
  (loop [byte 0 bits  (pad-bits-to-byte-size b 8) ]
    (if (empty? bits)
      byte
      (recur (bit-or (bit-shift-left byte 1) (first bits)) (rest bits)))))

(def bits-to-byte (memoize bits-to-byte!))

(defn- ^Integer bits-to-int! [b]
  (loop [int 0 bits  (pad-bits-to-byte-size b 32) ]
    (if (empty? bits)
      int
      (recur (bit-or (bit-shift-left int 1) (first bits)) (rest bits)))))

(def bits-to-int (memoize bits-to-int!))

(defn- byte-to-bits! [b]
  (loop [byt (short (bit-and b 0xFF)) bits (transient [])]
    (if (zero? byt)
      (reverse (pad-bits-to-byte-size (persistent! bits) 8))
      (recur (short (bit-and (unsigned-bit-shift-right byt 1) 0xFF)) (conj! bits (short (bit-and byt 0x01)))))))

(def byte-to-bits (memoize byte-to-bits!))

(defn- int-to-bits! [i]
  (loop [byt (int i) bits (transient []) ]
    (if (zero? byt)
      (reverse (pad-bits-to-byte-size (persistent! bits) 32))
      (recur (int (unsigned-bit-shift-right byt 1)) (conj! bits (int (bit-and byt 0x01)))))))

(def int-to-bits (memoize int-to-bits!))

(defn encode-bits-to-bytes [bits]
  (map bits-to-byte (partition 8 bits)))

(defn encode-bytes-to-bits [bytes]
  (map byte-to-bits bytes))
