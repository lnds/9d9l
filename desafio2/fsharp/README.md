# Weather

Esta es la implementación del desafío 2 en F#


## Configuración

Este programa requiere la librería Fsharp.Data. Yo la descargué de http://fsharp.github.io/FSharp.Data/index.html

Luego la coloqué en el directior FSharp.Data/bin.

Además, debes configurar la variable de entorno MONO_PATH

En mi caso hice:

	$ export MONO_PATH=FSharp.Data/bin:$MONO_PATH

Recuerda configurar la variable WEATHER_API_KEY

	$ export WEATHER_API_KEY=api-key-entregada-por-open-weather-map-org

## Ejecución


Para probarlo debes hacer:

	$ fsharpc -r FSharp.Data/bin/FSharp.Data.dll Weather.fs
	$ mono Weather.exe [-p] ciudad1 ciudad2 ciudad3 ... 

## Compilación

 	$ fsharpc -r FSharp.Data/bin/FSharp.Data.dll Weather.fs

Esto dejará un archivo .exe en el directorio actual.

## Uso

Generado el .exe puedes ejecutarlo directamente haciendo:

    $ mono Weather.exe [-p] ciudad1 ciudad2 ciudad3 ...


## Documentación

En el directorio doc se encuentra un documento que describe las particularidades de esta implementación.

## Licencia

Copyright © 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
