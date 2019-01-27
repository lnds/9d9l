import argparse
import sys
import time

BUFFER_SIZE = 65536
POS_VECTOR = 9
ELEMENTOS_VECTOR = 23
VECTOR_ELEMEN_SIZE = 6
VECTOR_SIZE = ELEMENTOS_VECTOR * VECTOR_ELEMEN_SIZE
CANTIDAD_INSTITUCIONES = 6
PERIODO_NULO = '000000'
LARGO_LINEA = POS_VECTOR+VECTOR_SIZE*CANTIDAD_INSTITUCIONES
TAM_RELLENO = ELEMENTOS_VECTOR*VECTOR_ELEMEN_SIZE
RELLENO = (' '*TAM_RELLENO)


def procesar(num_linea, linea):
    if len(linea) < LARGO_LINEA:
        sys.stderr.write(f"ERROR LARGO DE LINEA en linea {num_linea}")
        return linea
    encabezado = linea[0:POS_VECTOR]
    periodos = set()
    pos = POS_VECTOR
    while pos < LARGO_LINEA:
        periodo = linea[pos:pos+VECTOR_ELEMEN_SIZE]
        if periodo != PERIODO_NULO:
            periodos.add(periodo)
        pos += VECTOR_ELEMEN_SIZE
    periodos = list(periodos)
    periodos.sort(reverse=True)
    n = len(periodos)
    if n == 0:
        return encabezado + "N" + RELLENO
    elif n > ELEMENTOS_VECTOR:
        return encabezado + "S" + RELLENO
    relleno = ' '*(TAM_RELLENO-n*VECTOR_ELEMEN_SIZE)
    return encabezado + "D" + "".join(periodos) + relleno


inicio = time.time()
parser = argparse.ArgumentParser("Ordena un vector de números\n")
parser.add_argument('archivo_entrada',  type=argparse.FileType('r', bufsize=BUFFER_SIZE))
parser.add_argument('archivo_salida', nargs='?', type=argparse.FileType('wt', bufsize=BUFFER_SIZE), default=sys.stdout)


args = parser.parse_args()

for num_linea, linea in enumerate(args.archivo_entrada.readlines()):
    args.archivo_salida.write(procesar(num_linea, linea)+'\n')
args.archivo_salida.close()

fin = time.time()
duracion = fin - inicio
print("tiempo ocupado:", duracion)