# Vectore 

Esta es la implementación del desafío 3 en F#


## Configuración

Este programa requiere la librería Fsharp.Data. Yo la descargué de http://fsharp.github.io/FSharp.Data/index.html

Luego la coloqué en el directior FSharp.Data/bin.

Además, debes configurar la variable de entorno MONO_PATH

En mi caso hice:

	$ export MONO_PATH=FSharp.Data/bin:$MONO_PATH


## Ejecución


Para probarlo debes hacer:

	$ fsharpc -r FSharp.Data/bin/FSharp.Data.dll ordenar_vectores.fs
	$ mono ordenar_vectores.exe archivo_entrada archivo_salida

## Compilación

 	$ fsharpc -r FSharp.Data/bin/FSharp.Data.dll ordenar_vectores.fs

Esto dejará un archivo .exe en el directorio actual.

## Uso

Generado el .exe puedes ejecutarlo directamente haciendo:

    $ mono Weather.exe ordenar_vectores.exe archivo_entrada archivo_salida


## Documentación

En el directorio doc se encuentra un documento que describe las particularidades de esta implementación.

## Licencia

Copyright © 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
