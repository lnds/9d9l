# Huffman en Kotlin

Esta es la implementación del desafío 4 en Kotlin

## Preparación

Requieres Gradle o IntellijIDEA para compilar este proyecto.
A continuación documentamos el uso con Gradle.

## Ejecución

Para probar debes hacer:

    $ gradle run -pAppArgs.args="['-d|-c','archivo_entrada', 'archivo_salida']"
    
Como esto es engorroso hay un script en bash que se llama run.sh, en este caso basta con hacer

    $ run.sh [-c|-d] archivo_entrada archivo_salida
    
 ## Compilación
    
Simplemente invocar a gradle:
    
    $ gradle
        
Esto dejará un archivo llamado huffman-VERSION-all.jar en el directorio build/libs.
    
    
## Uso
    
Generado el archivo .jar puedes ejecutarlo directamente usando la JVM haciendo:
    
     $ java -jar build/libs/huffman-VERSION-all.jar [-c|-d] archivo_entrada archivo_salida
      
## Documentación

En el directorio doc se encuentra un documento que describe las particularidades de esta implementación.

## Licencia

Copyright © 2017 Eduardo Díaz

Distribuido bajo licencia MIT (ver archivo LICENSE para los detalles).