# OrdenarVector 

Este es el desafío 2 escrito en Haskell


## Configuración

Debes instalar Cabal (https://www.haskell.org/cabal/)

Luego debes insddtalar estos paquetes:

	cabal install clock
	cabal install formatting
	cabal install split

## Ejecución

Para probarlo puedes hacer:

	$ cabal run -- archivo_entrada archivo_salida

## Compilación

 	$ cabal build

Esto dejará un archivo ejecutable en el directorio dist/build/weather


## Uso

Generado el binario puedes ejecutarlo directamente haciendo:

    $ dist/build/ordenarvector/ordenarvector archivo_entrada archivo_salida

Recuerda configurar la variable de ambiente WEATHER_API_KEY para ejecutar el programa.


## Documentación

En el directorio doc se encuentra un documento que describe las particularidades de esta implementación.

## Licencia

Copyright © 2015, 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
