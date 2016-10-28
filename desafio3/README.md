# Vectores

Este desafío consiste en construir un filtro que reciba un archivo de vectores y los consolide en un archivo de salida.

La invocación del programa es la siguiente:

    $ ordenar_vector archivo_entrada archivo_salida

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

La salida estándar además debe desplegar la cantidad de líneas leidas y el tiempo, en segundos, empleado en procesar todo el archivo de entrada.


# Resultados

## Tiempos

Tiempos promedios, expresados en segundos, para procesar un archivo de 1 millón de vectores, la tercera columna es el tiempo en C dividido por el tiempo en el lenguaje respectivo. El tiempo promedio se sacó sobre una muestra de 5 ejecuciones del programa. La columna Proporción es la división del tiempo en el lenguaje sobre el tiempo original en C (mide cuantas veces es más rápido con respecto a C), mientras mayor este número significa que el programa es más rápido. La columna Proporción 2 mide la velocidad con respecto al programa en C optimizado (compilado con la opción -O3).

    | Lenguaje | Tiempo | Proporción | Proporción 2
    | C        | 11,56  |       1,00 | -
    | C-opt    |  4,53  |       2,55 | 1,00
    | Rust     |  5,53  |       2,09 | 0,82
    | Go       |  3,44  |       3,36 | 1,32
    | Haskell  | 10,15  |       1,14 | 0,45
    | Clojure  |  9,53  |       1,21 | 0,48
    | Scala    |  6,71  |       1,72 | 0,67
    | Swift    |  6,35  |       1,82 | 0,71

## Lineas de código

Calculadas usando la herramienta cloc (https://github.com/AlDanial/cloc)

    | C        |   90 |
    | Rust     |   90 |
    | Go       |   94 |
    | Haskell  |   62 |
    | Clojure  |   71 | 
    | Scala    |   82 | 
    | Swift    |  129 |

## Tiempo de Desarrollo

    Tiempos aproximados para desarrollar cada solución, medidos en horas:minutos, considera codificación, pruebas e investigación.
    Para medir estos tiempos usé la herramienta TimingApp para Mac OSX (http://TimingApp.com/)

    | C       | 1:45 |
    | Rust    | 7:23 |
    | Go      | 3:25 |
    | Haskell | 4:51 |
    | Clojure | 7:55 |
    | Scala   | 1:25 |
    | Swift   | 4:21 |

# Licencia

	(c) 2016 Eduardo Díaz.

	El código de este proyecto se distribuye bajo licencia MIT, ver el archivo LICENSE para los detalles.


