# Weather en Go

El nucleo de la operación concurrente está en las siguientes lineas:

	ch := make(chan WeatherReport)
	for _, city := range os.Args[2:] {
		go fetch(city, api_key, ch)
	}

(Vamos en los argumentos del 2 en adelante, porque el 1 es el parámetro -p).

La función fetch descarga de manera concurrente el reporte del tiempo para cada ciudad.

Al terminar fetch escribe el resultado en el canal ch.

Fetch en esencia hace lo siguiente:

	func fetch(city string, api_key string, ch chan <- WeatherReport) {
		url := make_url(city, api_key)
		resp, err := http.Get(url)
		...
		weather, success := ParseCurrentWeather(body)
		if success {
			return WeatherReport { "",  weather.City.Name, weather.Temperature.Max, weather.Weather.Value}
		} else {
			return WeatherReport { "error interpretando xml ", city, 0.0, ""}
		}	


# Parsing del XML

La api para hacer unmarshalling de XML es bastante util para este caso.

Este es un xml tipico de respuesta de la api:

	<current>
		<city id="4930956" name="Boston">
			<coord lon="-71.06" lat="42.36"/>
			<country>US</country>
			<sun rise="2016-03-25T10:36:48" set="2016-03-25T23:03:30"/>
			</city>
		<temperature value="7.61" min="1" max="19" unit="metric"/>
		<humidity value="86" unit="%"/>
		<pressure value="1008" unit="hPa"/>
		<wind>
			<speed value="1.5" name="Calm"/>
			<gusts/>
			<direction value="20" code="NNE" name="North-northeast"/>
		</wind>
		<clouds value="90" name="overcast clouds"/>
		<visibility/>
		<precipitation mode="no"/>
		<weather number="500" value="lluvia ligera" icon="10d"/>
		<lastupdate value="2016-03-25T16:06:00"/>
	</current>

Cómo sólo nos interesan los elementos <city>, <temperature> y <weather> solo definimos estos del siguiente modo:

	type City struct {
		Name string `xml:"name,attr"`
		Country string `xml:"country"`
	}

	type Weather struct {
		Value string `xml:"value,attr"`
	}

	type Temperature struct {
		Max float32 `xml:"max,attr"`
	}

	type Current struct {
		XMLName xml.Name `xml:"current"`
		City City `xml:"city"`
		Temperature Temperature `xml:"temperature"`
		Weather Weather `xml:"weather"`
	}

# Invocando la API

El código de fetch() y cur_fetch() podrían refactorizarse de algún modo.
Notar que fetch() tiene un loop para reintentar la descarga cuando esta falla, esto es porque OpenWeatherMap retorna un código 423 cuando se invoca muchas veces simultáneas la API. En este loop esperamos unos milisegundos y reintentamos.

