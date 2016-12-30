import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.io.IOException;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 *
 * @author MigSoft
 */
public class OrdenaVector {

    private static final int VECTOR = 6;
    private static final int MAX_VECTOR = 23;
    private static final int INICIO_CADENA = 9;
    private static final String RELLENO_BLANCO = fill("",VECTOR * MAX_VECTOR +1, ' ' );

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        Date inicio = new Date();
       
        if(args == null || args.length < 2){
            System.err.println("Error no se ingresaron argumentos requeridos");
            System.err.println("Ejemplo java OrdenaVector archivo_entrada archivo_salida");
            return;
        }

        File entrada = new File(args[0]);
        if(!entrada.isFile()){
            System.err.println("El archivo indicado no existe ("+args[0]+")");
            return;
        }
        File salida = new File(args[1]);
        
        OrdenaVector ordenaVector = new OrdenaVector();
        try {
            ordenaVector.procesaArchivos(entrada, salida);
        } catch (IOException ex) {
            Logger.getLogger(OrdenaVector.class.getName()).log(Level.SEVERE, null, ex);
        }

        Date fin = new Date();
        System.out.println("Tiempo ocupado: " + (fin.getTime() - inicio.getTime()));
    }

    public void procesaArchivos(File entrada, File salida) throws FileNotFoundException, IOException {
        try (BufferedWriter output = new BufferedWriter(new FileWriter(salida))) {
            Consumer<String> c = (linea) -> {
                try {
                    output.write(this.procesaLinea(linea));
                } catch (IOException ex) {
                    Logger.getLogger(OrdenaVector.class.getName()).log(Level.SEVERE, null, ex);
                }
            };

            try (InputStream fis = new FileInputStream(entrada)) {
                InputStreamReader isr = new InputStreamReader(fis);
                BufferedReader br = new BufferedReader(isr);
                br.lines().parallel().forEach(c);
            }
        }
    }

    public String procesaLinea(String linea) {
        int inicio = INICIO_CADENA;
        int fin = INICIO_CADENA + VECTOR;
        ArrayList<String> salida = new ArrayList();
        while (linea.length() > fin) {
            String fecha = linea.substring(inicio, fin);
            inicio += VECTOR;
            fin += VECTOR;
            salida.add(fecha);
        }
        int elementos = salida.size();
        if (elementos == 0) {//sin elementos
            return linea.substring(0, INICIO_CADENA) + "N\n";
        } else if (elementos > MAX_VECTOR) {//con mas de 23 elementos
            return linea.substring(0, INICIO_CADENA) + "S" + RELLENO_BLANCO + "\n";
        } else {
            String mergedString = salida.parallelStream().
                    filter(string -> !"000000".equals(string)).
                    sorted((e1, e2) -> e2.compareTo(e1)).                    
                    collect(Collectors.joining(""));
            return linea.substring(0, INICIO_CADENA) + "D" + mergedString + "\n";
        }
    }

    public static String fill(String strInit, int cantidadFill, char charFill) {
        StringBuilder padded = new StringBuilder(strInit);
        while (padded.length() < cantidadFill) {
            padded.append(charFill);
        }
        return padded.toString();
    }
}
