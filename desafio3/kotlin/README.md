# ordenar vectores en Kotlin

Esta es la implementación del desafío 3 usando Kotlin.

## Preparación

Se necesita Gradle para compilar este proyecto.

## Ejecución 

Para probar debes hacer:

    $ gradle run -pAppArgs.args="['archivo_entrada', 'archivo_salida']"
     
Como esto es bastante engorroso escribí un script en bash que se llama run.sh, en este caso basta con hacer

    $ run.sh archivo_entrada archivo_salida
    
    
    
## Compilación

Simplemente invocar a gradle:

    $ gradle
    
Esto dejará un archivo llamado vectores-VERSION-all.jar en el directorio actual.

## Uso

Generado el archivo .jar puedes ejecutarlo directamente usando la JVM haciendo:

    $ java -jar vectores-VERSION-all.jar archivo_entrada archivo_salida
  
## Documentación

En el directorio doc se encuentra un documento que describe las particularidades de esta implementación.

## Licencia

Copyright © 2015, 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
