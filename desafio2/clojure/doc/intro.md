# Weather en Clojure

La paralelización de esta tarea en Clojure es ridiculamente trivial.

Esta es la version secuencial:

      (map weather-api cities)

Esta es la versión paralela:

      (pmap weather-api cities)


El resto es imprimirlas ordenadamente, que es lo que hacemo con la ayuda de sort-reports



	(defn sort-reports [reports]
		sort-by :max-temp #(< 0 (compare %1 %2)) reports))


# Llamada a la API

Para usar la API de OpenWeatherMap se requiere una key, que se obtiene en su sitio.
El programa asume que hay una variable de entorno que contiene la key (WEATHER_API_KEY).

Si se llama a la API muchas veces esta responde con el codigo 429 (too many requests), por esto que la llamada a la API tiene reintentos con pausas de 150 milisegundos. Esto es muy importante cuando se ejecuta la versión concurrente del programa (con el parámetro -p).

# Parsing de XML

Usamos la api estandar de clojure (core/xml).

La siguiente función simplifica la tarea de analizar el map que clojure crea para el xml:


	(defn extract-tag [tag x]
		(first (filter #(= tag (:tag %)) (:content x))))

# Ejemplo

	$ lein run Boston Antofagasta Santiago Madrid Barcelona Concepcion Quito "Buenos Aires"
	Concepcion                     22,0   cielo claro
	Santiago                       21,0   cielo claro
	Antofagasta                    20,0   cielo claro
	Buenos Aires                   20,0   nubes
	Boston                         19,0   niebla
	Madrid                         17,0   nubes rotas
	Barcelona                      15,0   algo de nubes
	Quito                          14,8   cielo claro
	tiempo en generar el reporte: 0:00:4,102

	$ lein run -p Boston Antofagasta Santiago Madrid Barcelona Concepcion Quito "Buenos Aires"
	Concepcion                     22,0   cielo claro
	Santiago                       21,0   cielo claro
	Antofagasta                    20,0   cielo claro
	Buenos Aires                   20,0   nubes
	Boston                         19,0   niebla
	Madrid                         17,0   nubes rotas
	Barcelona                      15,0   algo de nubes
	Quito                          14,8   cielo claro
	tiempo en generar el reporte: 0:00:2,650