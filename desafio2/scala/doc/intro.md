# Weather en Scala

La unica diferencia entre la versión "paralela" de scala y la secuencial es el uso de una lista paralela.

La versión secuencial es en esencia:

	cities.map(city => apiCall(city))

Versus

	cities.par map (city => apiCall(city))

Las colecciones en Scala tienen una versión "paralela" a la que se accede con el método .par

# LLamada a la API

Usé la clase Try, una seudo monad que permite manejar excepciones de una manera bastante práctica.


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


El resultado de una llamada a la API se modela con este tipo de dato algebraico:

  sealed trait WeatherResult
  case class Error(error: String, city: String) extends WeatherResult
  case class WeatherReport(city: String, temp: Double, conditions: String) extends WeatherResult

Con esto lo que hacemos es aplicar la monad List para mapear los nombres de las ciudades a WeatherResult.

Nuestra solución es esencialmente funcional:

	Map : List[String] -> List[WeatherResult]


# Parsing de XML

El parsing de XML en Scala es bastante sencillo, si xml es la respuesta obtenida al llamar a la url, obtener los datos que nos interesan del xml se expresa de forma sencilla así:

    val city = xml \\ "city" \ "@name"
    val temp = xml \\ "temperature" \ "@max"
    val weather = xml \\ "weather" \ "@value"

Esto es muy similar a XPath.
