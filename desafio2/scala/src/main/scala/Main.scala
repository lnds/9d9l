import scala.io.Source
import scala.util._
import scala.xml._

object Main {

  val time_report_message = "tiempo ocupado para generar el reporte: "
 // val report_format = "%-30.30s max:%5.1f  min:%5.1f   actual: %5.1f %s"
  val no_cities_provided_message = "debe ingresar una lista de ciudades"

  val api_key = sys.env("WEATHER_API_KEY")
  val maxTries = 10
  val threadSleep = 100

  sealed trait Args
  case object NoArgs extends Args
  case class SeqArgs(args:List[String]) extends Args
  case class ParArgs(args:List[String]) extends Args

  def checkArgs(args: Array[String]) : Args = 
    if (args.size == 0) 
      NoArgs
    else if (args(0) != "-p")
        SeqArgs(args.toList)
    else if (args.toList.tail.isEmpty)
      NoArgs
    else
      ParArgs(args.toList.tail)

  def makeUrl(city:String) : String = s"http://api.openweathermap.org/data/2.5/weather?q=$city&mode=xml&units=metric&appid=$api_key&lang=sp"

  sealed trait WeatherResult
  case class Error(error: String, city: String) extends WeatherResult
  case class WeatherReport(city: String, temp: Double, min: Double, max: Double, conditions: String) extends WeatherResult

  def parseDouble(str: String) : Double = Try(str.toDouble).getOrElse(0.0)

  def parseApiResponse(city: String, xmlResponse: String): WeatherResult = {
    val xml = XML.loadString(xmlResponse)
    val current = xml \\ "current"
    if (current.isEmpty)
      Error("error parsing xml response", city)
    else {
      val city = xml \\ "city" \ "@name"
      val temp = xml \\ "temperature" \ "@value"
      val min  = xml \\ "temperature" \ "@min"
      val max  = xml \\ "temperature" \ "@max"
      val weather = xml \\ "weather" \ "@value"
      WeatherReport(city.text, parseDouble(temp.text), parseDouble(min.text), parseDouble(max.text), weather.text)
    }
  }

  def apiCall(city: String): WeatherResult = {
    val url = makeUrl(city)
    for (tries <- 1 to maxTries) {
      val response = Try(Source.fromURL(url))
      response match {
        case Success(rsp) =>
          return parseApiResponse(city, rsp.mkString)
        case Failure(e) =>
          Thread.sleep(threadSleep)
      }
    }
    Error("error descargando url", city)
  }

  def compareWeatherReports(rep1: WeatherResult, rep2: WeatherResult) : Boolean =
    (rep1, rep2) match {
      case (_, Error(_,_)) => false
      case  (Error(_,_),_) => false
      case (WeatherReport(_, temp1, _, _, _), WeatherReport(_, temp2, _, _, _))  => temp2 < temp1
    } 

  def printReports(reports:List[WeatherResult]) : Unit = 
    reports.sortWith(compareWeatherReports).foreach { 
      case Error(err, city) =>
        println(s"$city Error: $err")
      case WeatherReport(city, temp, min, max, conditions) =>
        println(f"$city%-30.30s max:$max%5.1f  min:$min%5.1f   actual: $temp%5.1f $conditions")
    }


  def seqFetch(cities: List[String]) : Unit = 
    printReports(cities.map(city => apiCall(city)))

  def parFetch(cities:List[String]) : Unit = {
    val reports = (cities.par map (city => apiCall(city))).toList
    printReports(reports)
  }

  def showTime(block : => Unit) : Unit = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val elapsed = t1 - t0
    val msecs = elapsed / 1000000
    val hours = msecs / 3600000
    val mins = (msecs % 3600000) / 60000
    val secs = ((msecs % 3600000) % 60000) / 1000.0

    println(f"$time_report_message $hours%d:$mins%02d:$secs%02.3f")
  }

  def main(args: Array[String]): Unit = 
    checkArgs(args) match {
      case NoArgs => println(no_cities_provided_message)
      case SeqArgs(argList) => showTime { seqFetch(argList) }
      case ParArgs(argList) => showTime { parFetch(argList) }
    } 

}