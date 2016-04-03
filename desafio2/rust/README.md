# weather

Esta es la implementación del desafío 2 usando Rust.

## Configuración en OSX

En Mac OSX hay que aplicar esto: 

	export OPENSSL_INCLUDE_DIR=/usr/local/Cellar/openssl/1.0.2d_1/include/

(ver https://github.com/servo/servo/issues/7303).

Recuerda configurar la variable WEATHER_API_KEY

	$ export WEATHER_API_KEY=api-key-entregada-por-open-weather-map-org


## Ejecución

Para probarlo puedes hacer:

	$ cargo run -- [-p] ciudad1 ciudad2...

## Compilación

 	$ cargo build --release

Esto dejará un archivo ejecutable con el nombre weather en el directorio target/release.


## Uso

Generado el binario  puedes ejecutarlo directamente haciendo:

    $ target/release/weather [-p]  ciudad1 ciudad2 ciudad3...


## Documentación

En el directorio doc se encuentra un documento que describe las particularidades de esta implementación.

## Licencia

Copyright © 2015, 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
