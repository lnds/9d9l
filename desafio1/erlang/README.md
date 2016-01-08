# toque-y-fama

Este es el juego de toque y fama escrito en Erlang.

## Ejecución

Para probarlo debes hacer:

	$ erlc toque_y_fama.erl
	$ erl -noshell -s toque_y_fama main -s init stop

## Compilación

 	$ erlc toque_y_fama.erl

Esto genera un archivo toque_y_fama.beam que puede ser una con la VM de Erlang.

## Uso

Generado el archivo .beam puedes ejecutarlo  haciendo:

    $  erl -noshell -s toque_y_fama main -s init stop


## Documentación

En el directorio doc se encuentra un documento que describe las particularidades de este implementación.

## Licencia

Copyright © 2015, 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
