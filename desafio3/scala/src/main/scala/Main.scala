import scala.io._
import scala.util._
import scala.collection.immutable.StringOps._
import scala.collection.mutable.ArrayBuffer
import java.io._

object Main {


  val bufferSize = 4096
  val encoding = "UTF-8"
  val time_report_message = "tiempo ocupado: "
  val tamPeriodo = 6
  val cantInstituciones = 6
  val tamVector = 23
  val largoEncabezado = 9
  val largoLinea = largoEncabezado + cantInstituciones * tamVector * tamPeriodo
  val tamRelleno = tamVector * tamPeriodo

  sealed trait Args
  case object BadArgs extends Args
  case class FileArgs(entrada:String, salida:String) extends Args

  def checkArgs(args: Array[String]) : Args = 
    if (args.size != 2) 
      BadArgs
    else 
        FileArgs(args(0), args(1))

  def esPeriodoValido(periodo: String) = periodo.exists(_ != '0')

  def ordenarPeriodos(linea: String) : String = {
    val encabezado = linea.slice(0, largoEncabezado)
    var periodos = new ArrayBuffer[String]()
    var i = largoEncabezado
    while (i < largoLinea) {
      val periodo = linea.slice(i, i+tamPeriodo)
      if (esPeriodoValido(periodo) && !periodos.contains(periodo))
        periodos += periodo
      i += tamPeriodo
    }

    periodos = periodos.sortWith(_ > _)
    
    val len = periodos.length
    encabezado + (
      if (len == 0) 
          "N" + (" " * tamRelleno)
      else if (len > tamVector)
          "S" + " " * tamRelleno
      else 
          "D" +  periodos.mkString("") + " "*(tamRelleno-len*tamPeriodo)
    )
  }

  def filtrarLinea(linea: String, nl: Long) : String = 
    if (linea.length() == largoLinea) 
      ordenarPeriodos(linea)
    else {
      println(f"!!! Largo incorrecto en linea: $nl%d\n")
      linea
    } 

  def procesarArchivos(entrada: String, salida: String) : Unit = {
    Try(Source.fromFile(new File(entrada), encoding, bufferSize)) match {
      case Failure(e) =>  println(f"No pudo abrir archivo $entrada%s, causa: $e%s")
      case Success(rdr) => 
        Try(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(salida)))) match {
          case Failure(e) => println(f"No pudo crear archivo $salida%s, causa: $e%s")
          case Success(wrt) => 
            rdr.getLines.zipWithIndex.foreach { 
              case (linea, nl) =>
                wrt.write( filtrarLinea(linea, nl) )
                wrt.newLine()
            }
            wrt.close()
        }
    }
  }

  def showTime(block : => Unit) : Unit = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val elapsed = t1 - t0
    val msecs = elapsed / 1000000
    val secs = msecs / 1000.0

    println(f"$time_report_message $secs%02.3f")
  }

  def main(args: Array[String]): Unit = 
    checkArgs(args) match {
      case BadArgs => println("Uso: ordenar_vector archivo_entrada archivo_salida")
      case FileArgs(entrada, salida) => showTime { procesarArchivos(entrada, salida) }
    } 

}