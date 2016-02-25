# Concurrent News Reader

El desafío consiste en una aplicación que descargue varias fuentes RSS (y Atom) de forma concurrente.

Se debe crear un programa que reciba a través de la línea de comandos una lista de urls, el programa debe descargar, concurrentemente todas las fuentes. 
De cada fuente extraer la fecha, el titular y el extracto de cada "noticia".
Debe generar como salida una lista de los 10 titulares más recientes (es decir, ordenados por fecha de manera descendente), moestrando para cada uno, el título, la fuente, la fecha y un extracto de no más de 3 líneas extensión.

Por ejemplo:
   

   $ crdr url1 url2 url3 url4

La salida debe ser:

    título: <titulo articulo 1>
    fuente: url2
    fecha: 2016-01-16 15:47:02
    ...
    ...
    ..

    titulo: <titulo articulo 2>
    fuente: url3
    fecha: 2016-01-16 15:45:01
    ....
    ...
    ..

    titulo: <titule articula 3>
    fuente: url1
    fecha: 2016-01-15 23:45:30
    ....
    ...
    ..

    Tiempo ocupado en descargar las noticias: hh:mm:ss.ms


El programa debe "limpiar" los tags HTML del texto. Interpretar los "entities html4"  traduciéndolos a carácteres utf-8 (por ejemplo, &iacute; debe quedar como í, &amp; como &).

El output debe ser generado en utf-8 y emitirse por la salida estándar.
Al final debe informar el tiempo usado en decargar todas las noticias.



## Las implementaciones
	
	Este desfío ha sido implementado en los lenguajes definidos en el proyecto "9 desafíos en 9 lenguajes": Clojure,Erlang, F#, Go, Haskell, Kotlin, Rust, Scala y Swift.

	Los detalles están descritos en cada archivo README para cada implementación.

# Resultados

## Lineas de código

  Calculadas usando la herramienta cloc (https://github.com/AlDanial/cloc)

    | Go      | 236 |
    | Clojure | 103 |  


## Tiempo de Desarrollo

   Tiempos aproximados para desarrollar cada solución, considera codificación, pruebas e investigación.
   Para medir estos tiempos usé la herramienta TimingApp para Mac OSX (http://TimingApp.com/)

    | Go      | 3:45 |
    | Clojure | 4:50 |

# Licencia

	(c) 2016 Eduardo Díaz.

	El código de este proyecto se distribuye bajo licencia MIT, ver el archivo LICENSE para los detalles.


