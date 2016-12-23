/**
 *
 * vercion 1.0 463148 ms 7,7mn
 *
 * @author MigSoft
 */
class OrdenaVector {
    static final int VECTOR=6
    static final int MAX_VECTOR=23
    static final int INICIO_CADENA=9
    
    static main(arg){
        def inicio = new Date()
        def achivoEntrada
        def archivoSalida
        
        if(arg.size()>1 && arg[0]){
            achivoEntrada = arg[0]
        }else{
            achivoEntrada = "D:\\Desarrollo\\EduardoDiaz\\desafio3\\vector100.txt"
        }
        
        if(arg.size()>2 && arg[1]){
            archivoSalida = arg[1]
        }else{
            archivoSalida = "D:\\Desarrollo\\EduardoDiaz\\desafio3\\salida100.txt"
        }
        
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
        while ((linea = br.readLine()) != null) {
            salidaArch << procesaLinea(linea)+"\n"
        }
    }
    
    def procesaLinea(linea){
        def inicio = INICIO_CADENA
        def fin = INICIO_CADENA+VECTOR-1
        def termino=false
        HashSet salida = []
        while(linea.size() > fin) {
            def fecha = linea[inicio..fin]
            if(!(fecha =~'000000'))
                 salida<<fecha;
            inicio+=VECTOR
            fin+=VECTOR
            if(salida.size() > MAX_VECTOR)
               fin += linea.size()
        }
        
        def letra = "D"
        if(salida.size()) {
            letra="N"
        } else if (salida.size() > MAX_VECTOR) {
            letra="S"
        }
        
        linea[0..INICIO_CADENA-1] + letra + salida.sort().reverse().join("")
    }
}