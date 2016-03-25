# Weather

El desafío consiste en una aplicación que obtenga el clima de distintas ciudades, usando la api de OpenWeatherMap.org de forma concurrente.

Se debe crear un programa que reciba a través de la línea de comandos una lista de ciudades, el programa debe descargar, concurrentemente de open weather map el informe de clima de las ciudades.
El resultado se debe ordenar de la mayor a menor temperatura.
El resultado debe contener, la ciudad, la temperatura máxima y  las condiciones de clima.
Al final debe informar el tiempo ocupado para descargar la información.
Si se pasa el parámetro -p el programa hace la consulta "en paralelo"

Por ejemplo:
   

   $ weather -p Berlin Santiago Boston Madrid

La salida debe ser:

    Santiago 27 Despejado
    Berlin   19 Nublado
    Madrid   18 Lluvia
    Boston   17 Parcialmente nublado

    Tiempo ocupado en descargar información : hh:mm:ss.ms


El output debe ser generado en utf-8 y emitirse por la salida estándar.


## Las implementaciones
	
	Este desfío ha sido implementado en los lenguajes definidos en el proyecto "9 desafíos en 9 lenguajes": Clojure,Erlang, F#, Go, Haskell, Kotlin, Rust, Scala y Swift.

	Los detalles están descritos en cada archivo README para cada implementación.

# Resultados

## Lineas de código

  Calculadas usando la herramienta cloc (https://github.com/AlDanial/cloc)



## Tiempo de Desarrollo

   Tiempos aproximados para desarrollar cada solución, considera codificación, pruebas e investigación.
   Para medir estos tiempos usé la herramienta TimingApp para Mac OSX (http://TimingApp.com/)


# Licencia

	(c) 2016 Eduardo Díaz.

	El código de este proyecto se distribuye bajo licencia MIT, ver el archivo LICENSE para los detalles.


