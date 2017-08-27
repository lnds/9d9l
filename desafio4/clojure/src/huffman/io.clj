(ns huffman.io
  (:import (java.nio.file Paths Files OpenOption Path)))

(use 'huffman.bits)


(defn get-path [filename]
  (Paths/get filename (make-array String 0)))

(defn read-bytes [filename]
  (Files/readAllBytes (get-path filename)))

(defn write-bytes [^String filename ^"[B" bytes]
  (Files/write ^Path (get-path filename) ^"[B" bytes ^"[Ljava.nio.file.OpenOption;" (make-array OpenOption 0)))

(defn write-encoded [filename  bits]
  (let [bytes (encode-bits-to-bytes bits)]
    (write-bytes filename (byte-array bytes))))