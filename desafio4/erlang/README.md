# Huffman en Erlang 

Esta es la implementación en Erlang del desafío 4.

## Ejecución

Para probarlo debes hacer:

	$ erlc huffman.erl
	$ erl -noshell -s huffman  main [c|d] archivo_entrada archivo_salida -s init stop

## Compilación

 	$ erlc huffman.erl

Esto genera un archivo vectores.beam que se ejecuta con la VM de Erlang.

## Uso

Generado el archivo .beam puedes ejecutarlo  haciendo:

	$ erl -noshell -s huffman  main [c|d] archivo_entrada archivo_salida -s init stop


## Documentación


En el directorio doc se encuentra un documento que describe las particularidades de este implementación.

## Licencia

Copyright © 2015, 2016, 2017 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
