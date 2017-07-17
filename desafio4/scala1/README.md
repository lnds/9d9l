# Huffman en Scala

Esta es la implementación del desafío 4 en Scala

## Preparación

Requieres SBT o IntellijeIdea para compilar este proyecto.
A continuación documento el uso con SBT.

## Ejecución

Para probar debes hacer:

    $ sbt "run [-c|-d] archivo_entrada archivo_salida"
    
## Compilación

    $ sbt assembly
    
Esto dejará un archivo .jar en el directorio target/scala-2.12

## Uso

Generado el archivo .jar puedes ejecutarlo directamente usando la JVM haciendo:

    $ java -jar target/scala-2.12/huffman-assembly-1.0.jar [-c|-d] archivo_entrada archivo_salida
    
## Licencia
    
Copyright (c) 2017 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).