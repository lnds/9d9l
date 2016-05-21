# Weather 

Este es el desafío 2 escrito en Haskell


## Configuración

Debes instalar Cabal (https://www.haskell.org/cabal/)

Luego debes instalar estos paquetes:

	cabal install clock
	cabal install formatting
	cabal install http-client
	cabal install monad-parallel

## Ejecución

Para probarlo puedes hacer:

	$ cabal run -- [-p] ciudad1 ciudad2 ... ciudadn

## Compilación

 	$ cabal build

Esto dejará un archivo ejecutable en el directorio dist/build/weather


## Uso

Generado el binario puedes ejecutarlo directamente haciendo:

    $ dist/build/weather/weather [-p] ciudad1 ciudad2 ....

Recuerda configurar la variable de ambiente WEATHER_API_KEY para ejecutar el programa.


## Documentación

En el directorio doc se encuentra un documento que describe las particularidades de esta implementación.

## Licencia

Copyright © 2015, 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
