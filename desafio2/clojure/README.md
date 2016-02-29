# crdr

Esta es la implementación de la descarga concurrente de noticias (rss o atom) usando Clojure.
Corresponde al desafío 2.

## Configuración

Todo está configurado en el archivo project.clj

## Ejecución

Para probarlo puedes hacer:

	$ lein run url1 url2 url3....

## Compilación

 	$ lein uberjar

Esto dejará un archivo .jar con el crdr-VERSION-standalone.jar en el directorio target/uberjar.

Donde VERSION es la versión definida en project.clj.

## Uso

	$ java -jar target/uberjar/crdr-VERSION-standalone.jar url url2 url3...
	
## Licencia

Copyright © 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).