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
    | C        |  1,69  |       1,00 | -
    | C-opt    |  1,61  |       1,06 | 1,00
    | Rust     |  1,36  |       1,25 | 1,17
    | Go       |  2,37  |       0,71 | 0,67
    | Haskell  |  6,81  |       0,25 | 0,23
    | Clojure  |  6,75  |       0,25 | 0,24
    | Scala    |  3,73  |       0,45 | 0,43
    | Swift    |  3,26  |       0,52 | 0,49
    | F#       |  7.27  |       0,23 | 0,22
    | Kotlin   |  3,10  |       0,55 | 0,51
    | Erlang   | 12,08  |       0,14 | 0,13
    | Elixir   | 14,33  |       0,12 | 0,11
    | Java     |  4,41  |       0,38 | 0,36
    | Python   | 37,29  |       0.05 | 0,04

## Lineas de código

Calculadas usando la herramienta cloc (https://github.com/AlDanial/cloc)

    | C        |   93 |
    | Rust     |   70 |
    | Go       |   97 |
    | Haskell  |   91 |
    | Clojure  |   73 | 
    | Scala    |   82 | 
    | Swift    |  135 |
    | F#       |   68 |
    | Kotlin   |   64 |
    | Erlang   |  149 |
    | Elixir   |  100 |
    | Java     |   90 |
    | Python   |   49 |

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
    | Erlang  | 8:45 |
    | Elixir  | 1:48 |

## Ranking

Ordenado del más rápido al más lento:

    |  # | lenguaje     | Tiempo |
    |  1 | Rust         |   1,36 |
    |  2 | C Optimizado |   1,61 |
    |  3 | C            |   1,69 |
    |  4 | Go           |   2,37 |
    |  5 | Kotlin       |   3,10 |
    |  6 | Swift        |   3,26 |
    |  7 | Scala        |   3.73 |
    |  8 | Java         |   4,41 |
    |  9 | Clojure      |   6,75 |
    | 10 | Haskell      |   6,81 |
    | 11 | F#           |   7,27 |
    | 12 | Erlang       |  12,08 |
    | 13 | Elixir       |  14,33 |
    | 14 | Python       |  37,29 |

Ordenados del más corto al más largo:

    # |  Lenguaje |   Lineas |
    1 |  Python   |     49   |
    2 |  Kotlin   |     64   |   
    3 |  F#       |     68   |
    4 |  Rust     |     70   |
    5 |  Clojure  |     73   |
    6 |  Scala    |     82   |
    7 |  Java     |     90   |
    8 |  Haskell  |     91   |
    9 |  C        |     93
   10 |  Go       |     97   |
   11 |  Elixir   |    100   |
   12 |  Swift    |    135   |
   13 |  Erlang   |    149   |

## Aportes

    Elixir: Eduardo Diaz
    Java  : Miguel Gonzalez
    Python: Eduardo Díaz

# Licencia

	(c) 2016, 2017, 2019 Eduardo Díaz.

	El código de este proyecto se distribuye bajo licencia MIT, ver el archivo LICENSE para los detalles.


