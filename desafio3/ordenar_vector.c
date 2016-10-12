#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

/**
 * Ordena vector 
 * Utiliza un sort de inserción para ordenar un archivo de entrada que contiene
 * el vector a ordenar.
 *
 *
 * Autor: Eduardo Díaz
 * Fecha: 6 de junio 2011
 * Modificado para el desafío 3 en agosto de 2016.
 * Para compilar basta hacer: dmc ordenar_vector.c
 *
 * Observaciones:
 * 	Falta completar con algunas validaciones de entorno, como la existencia de los archivos, espacio en disco, etc.
 *
 */
 
 

#define BUFFER_SIZE 4096
#define POS_VECTOR 9 // donde empieza el vector
#define ELEMENTOS_VECTOR 23
#define VECTOR_ELEM_SIZE 6 // tamano del elemento del vector
#define VECTOR_SIZE (ELEMENTOS_VECTOR)* (VECTOR_ELEM_SIZE) // cantidad de elementos del vector
#define CANTIDAD_INSTITUCIONES 6


char* procesar_linea(char* linea, int num_linea);

int main(int argc, char* argv[])
{
	char buf[BUFFER_SIZE];
	char ifbuf[BUFFER_SIZE];
	char ofbuf[BUFFER_SIZE];
	int lineas = 0;
	FILE* archivo_entrada;
	FILE* archivo_salida;
	clock_t start = clock();
	clock_t end;
	if (argc < 2) {
		fprintf(stderr, "Uso: ordena_vector archivo_entrada [archivo_salida]");
		return -1;
	}
	
	archivo_entrada = argc < 2 ? stdin : fopen(argv[1], "rt");
	archivo_salida  = argc < 3 ? stdout : fopen(argv[2], "wt");
		
	setvbuf(archivo_entrada, ifbuf, _IOFBF, BUFFER_SIZE);
	setvbuf(archivo_salida,  ofbuf, _IOFBF, BUFFER_SIZE);
	while (fgets(buf, BUFFER_SIZE, archivo_entrada)) {
		fputs ( procesar_linea(buf, ++lineas), archivo_salida );
	}
	end = clock();
	printf("Tiempo ocupado: %.2f segundos\n", (end-start)/(double)CLOCKS_PER_SEC);
	exit(0);
	return 0;
}




int ordena_vector(char* vector);

char* procesar_linea(char* linea, int num_linea)
{
	int tam_vector;
	if (strlen(linea) < (POS_VECTOR+1)+(VECTOR_SIZE)*CANTIDAD_INSTITUCIONES) {
		fprintf(stderr, "ERROR LARGO DE LINEA en linea %d\n", num_linea);
		return linea;
	}
	tam_vector = ordena_vector(linea+POS_VECTOR);	
	// corta vector
	if (tam_vector == 0) {
		linea[POS_VECTOR] = 'N';
		memset(linea+POS_VECTOR+1, ' ', VECTOR_SIZE);
	}
	else if (tam_vector > ELEMENTOS_VECTOR) {
		linea[POS_VECTOR] = 'S';
		memset(linea+POS_VECTOR+1, ' ', VECTOR_SIZE);
	} else {
		linea[POS_VECTOR] = 'D';
	}
	linea[POS_VECTOR+VECTOR_SIZE+1] = '\n';
	linea[POS_VECTOR+VECTOR_SIZE+2] = '\0';
	return linea;
}

int ordena_vector(char* vector)
{
	char vector_trabajo[VECTOR_SIZE*CANTIDAD_INSTITUCIONES][VECTOR_ELEM_SIZE];
	int i, j;
	int n = 0;
	char* p = vector;
	char* vend = vector+(VECTOR_SIZE*VECTOR_ELEM_SIZE);
	memset((char*)vector_trabajo, '0', VECTOR_SIZE*CANTIDAD_INSTITUCIONES*VECTOR_ELEM_SIZE);
	
	// por cada elemento del vector
	for (p = vector; p < vend && *p != ' '; p+= VECTOR_ELEM_SIZE)
	{
		// sort por insercion
		for (i = 0; i < n && strncmp(p, vector_trabajo[i], VECTOR_ELEM_SIZE) < 0; i++)
			;
        if (i == n) {
    		if (strncmp(p, vector_trabajo[n], VECTOR_ELEM_SIZE) != 0)
  		 		memmove(vector_trabajo[n++], p, VECTOR_ELEM_SIZE);
	  	}
	    else {			
	    	if (strncmp(p, vector_trabajo[i], VECTOR_ELEM_SIZE) != 0) {
	  			for (j = VECTOR_SIZE-1; j > i; j--)
	  				memmove((char*)vector_trabajo[j], vector_trabajo[j-1], VECTOR_ELEM_SIZE);
	  			memmove(vector_trabajo[i], p, VECTOR_ELEM_SIZE);
	  			n++;	
	  		}
	  	}
  	
	} // for
	
	memset(vector, ' ', VECTOR_ELEM_SIZE*VECTOR_SIZE+1);
	for (i = 0, p = vector+1; i < n; i++, p+= VECTOR_ELEM_SIZE)
		memmove(p, (char*)vector_trabajo[i], VECTOR_ELEM_SIZE);
	return n;
}