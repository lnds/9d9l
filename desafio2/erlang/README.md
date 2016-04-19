# Weather

Esta es la implementación en Erlang del desafío 2.

## Ejecución

Para probarlo debes hacer:

	$ erlc weather.erl
	$ erl -noshell -s weather main [[p] city1 city2 city3...] -s init stop

## Compilación

 	$ erlc weather.erl

Esto genera un archivo weeather.beam que se ejecuta con la VM de Erlang.

## Uso

Generado el archivo .beam puedes ejecutarlo  haciendo:

    $  erl -noshell -s weather main  [[p] city1 city2 city3...] -s init stop

Notar que p indica si queremos ejecutar en paralelo la descarga del clima.

## Documentación


En el directorio doc se encuentra un documento que describe las particularidades de este implementación.

## Licencia

Copyright © 2015, 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
