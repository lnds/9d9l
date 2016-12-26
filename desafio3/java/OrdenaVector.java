import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.logging.Level;
import java.util.logging.Logger;

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
        String achivoEntrada;
        String archivoSalida;

        if (args != null && args.length >= 1) {
            achivoEntrada = args[0];
        } else {
            achivoEntrada = "D:\\Desarrollo\\EduardoDiaz\\desafio3\\vector100.txt";
        }

        if (args != null && args.length >= 2) {
            archivoSalida = args[1];
        } else {
            archivoSalida = "D:\\Desarrollo\\EduardoDiaz\\desafio3\\salida100.txt";
        }

        OrdenaVector ordenaVector = new OrdenaVector();
        try {
            ordenaVector.procesaArchivos(achivoEntrada, archivoSalida);
        } catch (IOException ex) {
            Logger.getLogger(OrdenaVector.class.getName()).log(Level.SEVERE, null, ex);
        }

        Date fin = new Date();
        System.out.println("Tiempo ocupado: " + (fin.getTime() - inicio.getTime()));
    }

    public void procesaArchivos(String entrada, String salida) throws FileNotFoundException, IOException {
        String linea;
        try (InputStream fis = new FileInputStream(entrada)) {
            InputStreamReader isr = new InputStreamReader(fis);
            BufferedReader br = new BufferedReader(isr);
            File salidaArch = new File(salida);
            try (BufferedWriter output = new BufferedWriter(new FileWriter(salidaArch))) {
                while ((linea = br.readLine()) != null) {
                    output.write(procesaLinea(linea) + "\n");
                }
            }
            System.out.println("Salida: "+salidaArch.getAbsolutePath());
        }
    }

    public String procesaLinea(String linea) {
        int inicio = INICIO_CADENA;
        int fin = INICIO_CADENA + VECTOR;
        HashSet salida = new HashSet();
        while (linea.length() > fin) {
            String fecha = linea.substring(inicio, fin);
            inicio += VECTOR;
            fin += VECTOR;
            salida.add(fecha);
        }
        salida.remove("000000");
        int elementos = salida.size();
        if (elementos == 0) {//sin elementos
            return linea.substring(0,INICIO_CADENA)+"N";
        } else if (elementos > MAX_VECTOR) {//con mas de 23 elementos
            return linea.substring(0,INICIO_CADENA)+"S"+RELLENO_BLANCO;
        } else {
            ArrayList list = new ArrayList<>(Arrays.asList(salida.toArray()));
            Collections.sort(list);
            Collections.reverse(list);
            return linea.substring(0,INICIO_CADENA)+"D"+String.join("", list);
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
