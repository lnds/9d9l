
/**
 *
 * Version 1.0 463148 ms 7,7min
 * Version 2.0 311940 ms 5,2min
 * Version 3.0 305809 ms 5.0min
 * Version 4.0 40043  ms 0.6min
 * @author MigSoft
 */
class OrdenaVector {
    static final int VECTOR=6
    static final int MAX_VECTOR=23
    static final int INICIO_CADENA=9
    String rellenoBlanco = "".padRight( VECTOR * MAX_VECTOR +1, ' ' )
    
    static main(arg){
        def inicio = new Date()
        if(arg.size()>2 && arg[1]){
            println "Error no se ingresaron argumentos requeridos"
            println "Ejemplo groovy OrdenaVector [archivo entrada] [archivo salida]"
            return
        }
        
        def achivoEntrada = arg[0]
        def archivoSalida = arg[1]
        new OrdenaVector().procesaArchivos(achivoEntrada, archivoSalida)
        
        def fin = new Date()
        
        println "Tiempo ocupado: ${fin.time-inicio.time}"
    }
    
    def procesaArchivos(entrada, salida){
        String linea;
        InputStream fis = new FileInputStream(entrada);
        InputStreamReader isr = new InputStreamReader(fis);
        BufferedReader br = new BufferedReader(isr);
        File salidaArch = new File(salida)
        BufferedWriter output = new BufferedWriter(new FileWriter(salidaArch))
        while ((linea = br.readLine()) != null) {
            output << procesaLinea(linea)+"\n"
        }
    }
    
    def procesaLinea(linea){
        int inicio = INICIO_CADENA
        int fin = INICIO_CADENA+VECTOR-1
        HashSet salida = []
        while(linea.size() > fin) {
            def fecha = linea[inicio..fin]
            inicio+=VECTOR
            fin+=VECTOR
            salida << fecha
        }
        salida.remove("000000")
        int elementos = salida.size()
        if(!elementos) {//sin elementos
            "${linea[0..INICIO_CADENA-1]}N"
        } else if (elementos > MAX_VECTOR) {//con mas de 23 elementos
            "${linea[0..INICIO_CADENA-1]}S$rellenoBlanco"
        } else {
            "${linea[0..INICIO_CADENA-1]}D${salida.sort().reverse().join('')}"
        }
    }
}