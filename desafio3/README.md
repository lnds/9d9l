# Vectores

Este desafío consiste en construir un filtro que reciba un archivo de vectores y los consolide en un archivo de salida.

La invocación del programa es la siguiente:

    $ ordenar_vector archivo_entrada archivo_salida

Si no se le entregan argumentos al programa este debe salir con un mensaje de error.
Al finalizar debe desplegar el tiempo, en minutos y segundos, empleado en procesar todo el archivo de entrada.

La entrada consiste en un archivo en que cada línea se divide en:

    Encabezado: 9 dígitos
    Detalle: que consiste en 6 vectores
    Vector: que contiene en 23 elementos que corresponden a periodos calendario (mes de algún año)
    Periodo: un número de 6 dígitos, puede ser 000000 o un número de la forma AAAAMM donde AAAA es un año y MM un mes.

El script en Perl gendata.pl permite generar un archivo con estas características.

La operación que se debe realizar es la siguiente:

    1. Se deben consolidar todos los periodos de los 6 vectores en un vector de a lo más 23 elementos.
    2. Si los periodos se repiten se debe dejar sólo 1.
    3. Los periodos se deben ordenar de mayor a menor.
    4. La salida debe ser la siguiente:
        Encabezado: 9 dígitos que se copian de la entrada
        Marca: una letra que puede tener los valores S, N ó D.
        Vector: un vector de a lo más 23 periodos.

    5. Se debe considerar lo siguiente:
        5.1 Si el vector consolidado tiene más de 23 elementos se debe colocar la marca S y el vector se debe llenar de espacios en blanco.
        5.2 Si el vector consolidado tiene cero elementos (porque vienen sólo 0s en los periodos) se debe colocar la marca N.
        5.3 Si el vector tiene menos de 24 elementos se debe colocar la marca D.

El programa ordenar_vector.c implementa correctamente este comportamiento, se puede usar de referencia.


# Resultados

## Tiempos

Tiempos promedios, expresados en segundos, para procesar un archivo de 1 millón de vectores, la tercera columna es el tiempo en C dividido por el tiempo en el lenguaje respectivo. El tiempo promedio se sacó sobre una muestra de 5 ejecuciones del programa. La columna Proporción es la división del tiempo en el lenguaje sobre el tiempo original en C (mide cuantas veces es más rápido con respecto a C), mientras mayor este número significa que el programa es más rápido. La columna Proporción 2 mide la velocidad con respecto al programa en C optimizado (compilado con la opción -O3).

    | Lenguaje | Tiempo | Proporción | Proporción 2
    | C        |  8,80  |       1,00 | -
    | C-opt    |  3,54  |       2,48 | 1,00
    | Rust     |  3,99  |       2,20 | 0,89
    | Go       |  2,92  |       3,02 | 1,22
    | Haskell  |  8,05  |       1,09 | 0,44
    | Clojure  |  7.79  |       1,13 | 0,45
    | Scala    |  6,02  |       1,46 | 0,59
    | Swift    |  4.66  |       1,89 | 0,76
    | F#       |  8.64  |       1,02 | 0.41
    | Kotlin   |  3.81  |       2.31 | 0.93
    | Erlang   | 49.25  |       0.18 | 0.07

## Lineas de código

Calculadas usando la herramienta cloc (https://github.com/AlDanial/cloc)

    | C        |   90 |
    | Rust     |   90 |
    | Go       |   94 |
    | Haskell  |   62 |
    | Clojure  |   70 | 
    | Scala    |   82 | 
    | Swift    |  129 |
    | F#       |   52 |
    | Kotlin   |   64 |
    | Erlang   |   75 |

## Tiempo de Desarrollo

Tiempos aproximados para desarrollar cada solución, medidos en horas:minutos, considera codificación, pruebas e investigación.
Para medir estos tiempos usé la herramienta TimingApp para Mac OSX (http://TimingApp.com/)

    | C       | 1:45 |
    | Rust    | 7:23 |
    | Go      | 3:25 |
    | Haskell | 4:51 |
    | Clojure | 8:15 |
    | Scala   | 1:25 |
    | Swift   | 4:21 |
    | F#      | 6:43 |
    | Kotlin  | 1:34 |
    | Erlang  | 7:45 |

## Ranking

Ordenados del más corto al más largo:

    # |  Lenguaje |   Lineas |
    1 |  F#       |     52   |
    2 |  Haskell  |     62   |   
    3 |  Kotlin   |     64   |
    4 |  Clojure  |     70   |
    5 |  Erlang   |     75   |
    6 |  Scala    |     82   |
    7 |  C        |     90   |
    8 |  Rust     |     90   |
    9 |  Go       |     94   |
   10 |  Swift    |    129   |

## Aportes

    

# Licencia

	(c) 2016 Eduardo Díaz.

	El código de este proyecto se distribuye bajo licencia MIT, ver el archivo LICENSE para los detalles.


