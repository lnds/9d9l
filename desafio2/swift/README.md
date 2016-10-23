# weather

Este es el desafío 2 escrito en Swift.

## Configuración

Recuerda configurar la variable WEATHER_API_KEY

	$ export WEATHER_API_KEY=api-key-entregada-por-open-weather-map-org

## Ejecución

Debes instalar la última versión desde el sitio swift.org de acuerdo a lo que sale en esta página: https://swift.org/getting-started/.

Este programa fue probado con Swift 2.2 (XCode 7+) en Mac OSX

Para probarlo puedes hacer:

	$ swift build 
	$ .build/debug/weather [-p] ciudad1 ciudad2 ciudad3 ..

## Compilación

 	$ swift build --configuration release

Esto dejará un archivo binario en .build/release

## Uso

Generado el archivo binario puedes ejecutarlo directamente haciendo:

	$ .build/release/weather [-p] ciudad1 ciudad2 ciudad3 ..


## Documentación

En el directorio doc se encuentra un documento que describe las particularidades de esta implementación.

## Licencia

Copyright © 2015, 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
