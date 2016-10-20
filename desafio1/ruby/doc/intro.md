# Toque y fama en Ruby
## por Aldrin Martoq

Ruby es uno de mis lenguajes favoritos, pues te permite crear código bastante elegante a todo nivel: desde un script sencillo, una biblioteca compartida hasta una aplicación compleja. Tiene elementos de Smalltalk (envío de mensajes versus invocar métodos), perl y varios otros lenguajes más, así como también puedes hacer programación funcional gracias a los bloques de código. Su única desventaja es el bajo rendimiento en CPU.


## Descripción de la solución

Lo primero a notar es que podemos usar unicode en las variables, en este caso `tamaño`.

```ruby
    tamaño = 5
    secuencia = (0..9).to_a.shuffle.first(tamaño)
```

La secuencia aleatoria es creada utilizando principios de programación funcional, podemos entender mejor si intentamos en una consola `irb` lo siguiente:

```ruby
    # primero creamos un rango de 0 al 9
    2.3.1 :001 > (0..9)
     => 0..9 
    # un rango es un objeto Lazy, con .to_a se invoca el generador retornado un arreglo de números
    2.3.1 :002 > (0..9).to_a
     => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] 
    # shuffle nos entrega un nuevo arreglo con los elementos desordenados
    2.3.1 :003 > (0..9).to_a.shuffle
     => [4, 2, 8, 9, 6, 7, 0, 1, 3, 5] 
    # first nos entrega un nuevo arreglo con los n-ésimos primeros elementos
    2.3.1 :004 > (0..9).to_a.shuffle.first(tamaño)
     => [7, 9, 2, 3, 0]
    # finalmente asignamos el resultado a la variable secuencia
    # cada vez nos queda una secuencia distinta pues se vuelve a ejecutar todo de nuevo
    2.3.1 :005 > secuencia = (0..9).to_a.shuffle.first(tamaño)
     => [4, 5, 1, 7, 8] 
```

Posteriormente mostramos las instrucciones con `puts` utilizando un *heredoc*, una técnica bastante usada en shell scripts en donde escribimos texto directamente hasta el delimitador, en este caso `FIN_TEXTO_BIENVENIDA`. Este texto permite *string interpolation*, es decir, puedes insertar texto utilizando código entre #{}.

```ruby
    puts <<FIN_TEXTO_BIENVENIDA
    #{secuencia}
    Bienvenido a Toque y Fama.
    ==========================

    En este juego debes tratar de adivinar una secuencia de #{tamaño} dígitos generadas por el programa.
    Para esto ingresas #{tamaño} dígitos distintos con el fin de adivinar la secuencia.
    Si has adivinado correctamente la posición de un dígito se produce una Fama.
    Si has adivinado uno de los dígitos de la secuencia, pero en una posición distinta se trata de un Toque.

    Ejemplo: Si la secuencia es secuencia: [8, 0, 6, 1, 3] e ingresas 40863, entonces en pantalla aparecerá:
    tu ingresaste [4, 0, 8, 6, 3]
    resultado: 2 Toques 2 Famas
    FIN_TEXTO_BIENVENIDA
```

El resto del programa es bastante simple, un ciclo infinito que cuenta los intentos. Si sale del ciclo, asumimos que el usuario ganó. Lo primero relevante es obtener el intento del usuario desde consola, y al mismo tiempo aprovechamos de verificar si debemos terminar el programa con `exit`.

```ruby
    if (linea = gets).nil? or linea.chomp! == 'salir'
      puts "\ngracias por jugar, adios."
      exit
    end
```

En la primera parte de la expresión, `gets` obtiene una línea desde la consola y esta es asignada a la variable linea en `linea = gets`. Una ventaja de ruby es que cada instrucción retorna siempre un objeto, por lo que si ponemos esto entre paréntesis podemos utilizar inmediatamente el valor ingresado sin instrucciones adicionales. Otra ventaja es que todo es un objeto, incluido `nil` el cual es equivalente a `NULL`. Y como todo es un objeto, le puedes preguntar si dicho objeto es nil enviando el mensaje `.nil?` el cual retorna `true` o `false`. En mi opinión esto se lee mejor que `(linea = gets) == nil`.

¿Por qué necesitamos verificar si gets entregó `nil`? Esto ocurre cuando el usuario presiona `CTRL + D`, es decir, cuando se cierra la entrada estándar.

En la segunda parte de la expresión, utilizamos `.chomp!` para quitar el fin de línea que recibimos desde la consola (el ENTER). Una convención en Ruby es que si un método cambia un objeto (lo muta) dicho método termina en `!`. Si no existiera este método, podríamos escribir lo siguiente que es equivalente, pero menos elegante:

```ruby
    if (linea = gets).nil? or (linea = linea.chomp) == salir
```

Ahora que tenemos la entrada en un string, nos interesa llevarla al mismo formato en que tenemos la secuencia a adivinar, para posteriormente contar los toques y famas. Podemos nuevamente experimentar en la consola `irb` lo siguiente:

```ruby
    # supongamos que el usuario ingreso este texto, ojo que ya quitamos el fin de línea
    2.3.1 :006 > linea = "1134a"
     => "1134a" 
    # gsub nos permite utilizar una expresión regular para retornar un nuevo string
    # en este caso cualquier caracter no numérico es reemplazado por un string vacío
    2.3.1 :007 > linea.gsub(/[^\d]/, '')
     => "1134" 
    # split divide un string en un arreglo dado un separador
    # como el separador es un string vacío, el resultado es un arreglo con cada caracter
    2.3.1 :008 > linea.gsub(/[^\d]/, '').split('')
     => ["1", "1", "3", "4"] 
    # ahora tenemos un arreglo pero con elementos strings, utilizamos map el cual retorna un nuevo arreglo
    # map usualmente recibe un bloque de código, por ejemplo lo siguiente
    2.3.1 :010 > linea.gsub(/[^\d]/, '').split('').map { |elemento| elemento.to_i }
     => [1, 1, 3, 4] 
    # pero podemos acortarlo enviando de parámetro &:to_i, lo cual invocará dicho método en cada elemento
    # el resultado es que se reemplaza cada elemento del arreglo por lo retornado de to_i de cada elemento
    # es decir, transformamos el arreglo de strings a un arreglo de números
    2.3.1 :010 > linea.gsub(/[^\d]/, '').split('').map(&:to_i)
     => [1, 1, 3, 4] 
    # finalmente con uniq quitamos los números que están repetidos en el arreglo
    2.3.1 :011 > linea.gsub(/[^\d]/, '').split('').map(&:to_i).uniq
     => [1, 3, 4] 
```

Notar que no es necesario agregar `return` a los bloques de código, en Ruby return es implícito.

En este caso tenemos una entrada inválida, y la validamos simplemente con `numero.length != tamaño`, es decir esperábamos 5 números pero terminamos con 3 después de todo el proceso. La segunda expresión `numero.join != linea` tiene el objeto de tomar como error la entrada "12345a" por ejemplo, ya que recuerden que con gsub quitamos los caracteres no numéricos. Para ello se transforma el arreglo a un string con join y se compara que sea lo mismo que la linea ingresada por el usuario.

Finalmente para calcular las famas, enviamos `each_with_index` al arreglo `numero`, el cual nos genera un iterador que invocará a la siguiente función enviando el elemento y el índice del mismo en el arreglo. por ejemplo:

```ruby
    2.3.1 :016 > numero = [1, 3, 7, 6, 9]
     => [1, 3, 7, 6, 9] 
    2.3.1 :017 > numero.each_with_index { |elemento, indice| puts "indice: #{indice} - elemento: #{elemento}" }
    indice: 0 - elemento: 1
    indice: 1 - elemento: 3
    indice: 2 - elemento: 7
    indice: 3 - elemento: 6
    indice: 4 - elemento: 9
     => [1, 3, 7, 6, 9] 
```

Ahora `select` lo que hace es retornar un nuevo arreglo con los elementos que cumplan con alguna condición. Por ejemplo, si quisieramos los números pares de un rango:

```ruby
    # obtenemos los 10 elementos
    2.3.1 :018 > (0..9).to_a
     => [0, 1, 2, 3, 4, 5, 6, 7, 8, 9] 
    # seleccionamos los elementos pares
    2.3.1 :018 > (0..9).select { |x| x % 2 == 0 }
     => [0, 2, 4, 6, 8] 
    # lo mismo pero mas corto, pues even? en cada objeto retorna si es par o no
    2.3.1 :018 > (0..9).select { |x| x.even? }
     => [0, 2, 4, 6, 8] 
    # o mejor aún
    2.3.1 :020 > (0..9).select(&:even?)
     => [0, 2, 4, 6, 8] 
```

En nuestro código al unir `each_with_index` con `select`, podemos utilizar una expresión que retorna `true` si tanto el elemento como la posición del arreglo `numero` coincide con el del arreglo `secuencia`. Es decir, seleccionamos todas las famas que hay en el arreglo `numero`. Finalmente como las famas es la cantidad de coincidencias, simplemente contamos cuantos elementos tiene el arreglo con `count`.

```ruby
    # vamos a asignar secuencia y numero a mano primero
    2.3.1 :027 > secuencia = [0, 3, 7, 6, 8]
     => [0, 3, 7, 6, 8] 
    2.3.1 :028 > numero = [1, 3, 7, 6, 9]
     => [1, 3, 7, 6, 9] 
    # notar que each_with_index también es un generador
    2.3.1 :029 > numero.each_with_index
     => #<Enumerator: [1, 3, 7, 6, 9]:each_with_index> 
    # para ver mejor qué recibirá select, podemos transformarlo en un arreglo
    2.3.1 :030 > numero.each_with_index.to_a
     => [[1, 0], [3, 1], [7, 2], [6, 3], [9, 4]] 
    # entonces obtenemos un arreglo con los pares [elemento, indice] que son famas
    2.3.1 :031 > numero.each_with_index.select { |n, i| secuencia[i] == n }
     => [[3, 1], [7, 2], [6, 3]] 
    # finalmente la cantidad de elementos del arreglo serán las famas
    2.3.1 :032 > numero.each_with_index.select { |n, i| secuencia[i] == n }.count
     => 3
```

El caso de los toques es similar pero más sencillo:

```ruby
    toques = numero.select { |n| secuencia.include? n }.count
    toques -= famas
```

Ahora que entendemos todo podemos hablar en español: seleccionamos del arreglo `numero` aquellos elementos que están presentes en el arreglo secuencia y luego contamos cuantos son, es decir, los toques. Ojo que esta cuenta también incluirá a las famas, entonces quitamos a toques la cantidad de famas que encontramos.

Implementar el desafío desde clonar el repositorio hasta terminar la primera versión tomó el siguiente tiempo:

    ini: Oct 19 22:43:45 2016
    fin: Oct 19 23:54:49 2016
     ∆t: 1h 11m 4s

Esta documentación tomó un tiempo similar. Espero que lo encuentren útil. Saludos,

-- Aldrin.
