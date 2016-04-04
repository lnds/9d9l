# weather

Esta es la implementación del desafío 2 usando Go.

## Configuración

Recuerda configurar la variable WEATHER_API_KEY

	$ export WEATHER_API_KEY=api-key-entregada-por-open-weather-map-org

## Ejecución

Configurar adecuadamente el ambiente Go (leer How to Write Go Code https://golang.org/doc/code.html)

Para probarlo puedes hacer:

	$ go run weather.go api.go [-p] ciudad1 ciudad2...

## Compilación

 	$ go build -o weather

Esto dejará un archivo ejecutable con el nombre weather en el directorio actual.


## Uso

Generado el binario  puedes ejecutarlo directamente haciendo:

    $ ./weather [-p]  ciudad1 ciudad2 ciudad3...


## Documentación

En el directorio doc se encuentra un documento que describe las particularidades de esta implementación.

## Licencia

Copyright © 2015, 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
