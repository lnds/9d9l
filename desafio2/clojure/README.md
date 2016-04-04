# weather

Esta es la implementación de la consulta del tiempo usando Clojure.
Corresponde al desafío 2.

## Configuración

Todo está configurado en el archivo project.clj

Recuerda configurar la variable WEATHER_API_KEY

	$ export WEATHER_API_KEY=api-key-entregada-por-open-weather-map-org

## Ejecución

Para probarlo puedes hacer:

	$ lein run [-p] ciudad1 ciudad2 ciudad3

## Compilación

 	$ lein uberjar

Esto dejará un archivo .jar con el nombre  weather-VERSION-standalone.jar en el directorio target/uberjar.

Donde VERSION es la versión definida en project.clj.

## Uso

	$ java -jar target/uberjar/weather-VERSION-standalone.jar [-p] ciudad1 ciudad2 ciudad3 ...
	
## Licencia

Copyright © 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).