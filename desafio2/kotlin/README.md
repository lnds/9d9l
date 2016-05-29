# weather

Esta es la implementación del desafío 2 usando Kotlin.

## Preparación

Se necesita Gradle para compilar este proyecto.

## Ejecución 

Para probar debes hacer:

    $ gradle run -pAppArgs.args="['-p','Ciudad1',... Ciudad]"
     
Como esto es bastante engorroso escribí un script en bash que se llama run.sh, en este caso basta con hacer

    $ run.sh -p Ciudad1 Ciudad2
    
    
Por supuesto -p es opcional.

    
## Compilación

Simplemente invocar a gradle:

    $ gradle
    
Esto dejará un archivo llamado weather-VERSION-all.jar en el directorio actual.

## Uso

Generado el archivo .jar puedes ejecutarlo directamente usando la JVM haciendo:

    $ java -jar weather-VERSION-all.jar [-p] Ciudad1 ....
  
## Documentación

En el directorio doc se encuentra un documento que describe las particularidades de esta implementación.

## Licencia

Copyright © 2015, 2016 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).
