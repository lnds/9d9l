package weather

import java.net.URL
import kotlin.io.*
import org.w3c.dom.*
import javax.xml.parsers.*
import java.io.*
import java.nio.charset.Charset

val noCitiesProvidedMessage = "debe ingresar una lista de ciudades"
val timeReportMessage = "tiempo ocupado para generar el reporte: "

val apiKey = System.getenv("WEATHER_API_KEY")
val maxTries = 10
val threadSleep = 100L

interface Args
object NoArgs : Args
data class SeqArgs(val args:List<String>) : Args
data class ParArgs(val args:List<String>) : Args

interface WeatherResult : Comparable<WeatherResult>

data class Error(val error:String, val city:String) : WeatherResult  {
    override fun compareTo(other:WeatherResult) : Int {
        return -1
    }
}

data class WeatherReport(val city:String, val temp:Double, val min:Double, val max:Double, val conditions:String) : WeatherResult {
    override fun compareTo(other:WeatherResult) : Int {
        when (other) {
            is Error -> return 1
            is WeatherReport -> {
                return if (other.temp > this.temp)  1 else -1
            }
        }
        return 0
    }
}

fun makeUrl(city:String) : String {
    return  "http://api.openweathermap.org/data/2.5/weather?q=${city}&mode=xml&units=metric&appid=${apiKey}&lang=sp"
}

fun apiCall(city:String) : WeatherResult {
    val url = makeUrl(city)
    for (tries in 1..10) {
        try {
            val req = URL(url)
            val rsp = req.readText()
            return parseApiResponse(city, rsp)
        } catch (e:Exception) {
            Thread.sleep(threadSleep)
        }
    }
    return Error("error descargando url", city)
}


@Suppress("UNUSED_EXPRESSION")
fun parseApiResponse(city: String, response:String) : WeatherResult {
    val factory = DocumentBuilderFactory.newInstance()
    val builder = factory.newDocumentBuilder()
    val xmlStrBuilder = StringBuilder()
    xmlStrBuilder.append(response)
    val str = xmlStrBuilder.toString()
    val bytes = str.toByteArray(Charset.forName("UTF-8"))
    val input = ByteArrayInputStream(bytes)
    val doc = builder.parse(input)
    val root = doc.documentElement
    val cityName = root.getElementsByTagName("city").item(0).attributes.getNamedItem("name").textContent
    val temp = root.getElementsByTagName("temperature").item(0).attributes.getNamedItem("value").textContent
    val min = root.getElementsByTagName("temperature").item(0).attributes.getNamedItem("min").textContent
    val max = root.getElementsByTagName("temperature").item(0).attributes.getNamedItem("max").textContent
    val conditions = root.getElementsByTagName("weather").item(0).attributes.getNamedItem("value").textContent
    return WeatherReport(cityName, temp.toDouble(), min.toDouble(), max.toDouble(), conditions)
}

fun checkArgs(args:Array<String>) : Args {
    if (args.isEmpty())
        return NoArgs
    else if (args[0] != "-p")
        return SeqArgs(args.toList())
    else if (args.toList().size == 1)
        return NoArgs // -p sin argumentos
    else
        return ParArgs(args.toList().drop(1))
}

@Suppress("UNUSED_EXPRESSION")
fun showTime(block: () -> Unit) {
    val t0 = System.nanoTime()
    block()
    val t1 = System.nanoTime()
    val elapsed = t1 - t0
    val msecs = elapsed / 1000000
    val hours = msecs / 3600000
    val mins = (msecs % 3600000) / 60000
    val secs = ((msecs % 3600000) % 60000) / 1000.0

    println(timeReportMessage+ " " + "%d:%02d:%02.3f".format(hours, mins, secs))
}

fun printReports(reports:List<WeatherResult>) {
    reports.sorted().forEach { r ->
        when (r) {
            is Error -> println("${r.city} Error: ${r.error}")
            is WeatherReport -> println("%-30.30s max:%5.1f  min:%5.1f   actual: %5.1f %s".format(r.city, r.max, r.min, r.temp, r.conditions))
        }

    }
}

fun seqFetch(sa:SeqArgs) {
    val cities = sa.args
    val reports = cities.map(::apiCall)
    printReports(reports)
}

fun parFetch(pa:ParArgs) {
    println("par ${pa.args}")
}


fun main(args: Array<String>)  {
    val l = checkArgs(args)
    when (l) {
        is NoArgs -> println(noCitiesProvidedMessage)
        is SeqArgs -> showTime { seqFetch(l) }
        is ParArgs -> showTime { parFetch(l) }
    }
}