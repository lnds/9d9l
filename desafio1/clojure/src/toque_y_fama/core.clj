(ns toque-y-fama.core (:gen-class))

(defn mostrar-reglas [tam]
  (print "Bienvenido a Toque y Fama.
==========================\n
En este juego debes tratar de adivinar una secuencia de" tam "dígitos generadas por el programa.
Para esto ingresas" tam "dígitos distintos con el fin de adivinar la secuencia.
Si has adivinado correctamente la posición de un dígito se produce una Fama.
Si has adivinado uno de los dígitos de la secuencia, pero en una posición distinta se trata de un Toque.

Ejemplo: Si la secuencia es secuencia: [8, 0, 6, 1, 3] e ingresas 40863, entonces en pantalla aparecerá
tu ingresaste [4, 0, 8, 6, 3]
resultado: 2 Toques 2 Famas\n\n"))

(defn ingresar [tam]
  (println "Ingresa una secuencia de" tam "dígitos distintos (o escribe salir):")
  (read-line))

(defn famas [num sec] 
  (count (filter #(= (get % 0) (get % 1)) (map vector num sec))))

(defn toques [num sec]
  (count (filter #(contains? (set sec) %) num)))

(defn solo-digitos [num]
  (every? #(Character/isDigit %) num))
 
 (defn largo-esperado [tam num]
  (let [cuenta-unicos (count (distinct num))]
    (and 
      (= tam cuenta-unicos)
      (= (count num) cuenta-unicos))))
  
(defn validar [tam num]
  (if (and  (solo-digitos num) (largo-esperado tam num))    
    (map #(Character/digit % 10) (seq num))
    nil))

(defn gano? [num sec tam]  
  (let [f (famas num sec) t (- (toques num sec) f)]
   (println "tu ingresaste" num) 
   (println "resultado:" t "Toques, " f "Famas\n")
   (= tam f)))
                 
(defn -main
  "Juego de Toque y Fama"
  [& args]
  (def tam 5)
  (def sec (take tam (shuffle (range 10))))
  (mostrar-reglas tam)
  (loop [intentos 1 accion (ingresar tam)]
        (if (or (= accion "salir") (nil? accion)) ; si es salir o eof
          (println "\ngracias por jugar, adios.")
          (let [num (validar tam accion)]
            (if (nil? num)
              (do (println "error!\n") (recur (inc intentos) (ingresar tam)))
              (if (gano? num sec tam)
                (println "Ganaste! Acertaste al intento " intentos "! La secuencia era " sec ".")
                (recur (inc intentos) (ingresar tam))))))))