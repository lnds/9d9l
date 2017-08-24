(ns huffman.io
  (:import (java.nio.file Paths)))

(defn get-path [filename]
  (Paths/get filename (make-array String 0)))

(defn read-bytes [filename]
  (java.nio.file.Files/readAllBytes (get-path filename)))