# Huffman en Clojure

Esta es la implementación del desafío 4 en Clojure

## Preparación

Requieres Leiningen (ver http://leiningen.org/).
A continuación documento el uso con SBT.

## Ejecución

Para probar debes hacer
        
    $ lein run [-c|-d] archivo_entrada archivo_salida
    
## Compilación

    $ lein uberjar

Esto dejará un archivo .jar con el nombre  huffman-VERSION-standalone.jar en el directorio target/uberjar.

## Uso

Generado el archivo .jar puedes ejecutarlo directamente usando la JVM haciendo:

    $ java -jar target/uberjar/huffman-VERSION-standalone.jar [-c|-d] archivo_entrada archivo_salida
    
## Licencia
    
Copyright (c) 2017 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
