# crdr

Esta es la implementación de la descarga concurrente de noticias (rss o atom) usando Go.
Corresponde al desafío 2.

## Configuración

Antes de usarlo se debe hacer:


	$ go get golang.org/x/net/html

Para poder instalar ese paquete. Este paquete es usado para "parsear" el html obtenido en el contenido de la noticia.

## Ejecución

Configurar adecuadamente el ambiente Go (leer How to Write Go Code https://golang.org/doc/code.html)

Para probarlo puedes hacer:

	$ go run rss.go crdr.go url1 url2 url3....

## Compilación

 	$ go build -o crdr

Esto dejará un archivo ejecutable con el nombre crdr en el directorio actual.


## Uso

Generado el binario  puedes ejecutarlo directamente haciendo:

    $ ./crdr url1 url2 urls...


## Documentación

En el directorio doc se encuentra un documento que describe las particularidades de esta implementación.

## Licencia

Copyright © 2015, 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
