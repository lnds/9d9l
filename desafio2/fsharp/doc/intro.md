# Weather en F#

La versión secuencial del algoritmo es:

	let seq_fetch cities = cities |> List.map api_call

Para ejecutarlo en paralelo usamos Async.Parallel:

	let par_fetch cities = cities |> List.map api_call_async  |> Async.Parallel |> Async.RunSynchronously  |> List.ofArray

# Parsing del XML y llamada a la API

Para hacer parsing de XML usamos  FSharp.Data, la documentación está disponible en http://fsharp.github.io/FSharp.Data/

Para hacer el parsing primero declaramos un tipo del siguiente modo:

	type Result = XmlProvider<"""<current>
                            <city id="4930956" name="Boston">
                            <coord lon="-71.06" lat="42.36"/>
                            <country>US</country>
                            <sun rise="2016-04-03T10:21:21" set="2016-04-03T23:13:38"/>
                            </city>
                            <temperature value="1.24" min="-2.0" max="9.0" unit="metric"/>
                            <humidity value="31" unit="%"/>
                            <pressure value="1017" unit="hPa"/>
                            <wind>
                            <speed value="6.2" name="Moderate breeze"/>
                            <gusts value="8.7"/>
                            <direction value="350" code="" name=""/>
                            </wind>
                            <clouds value="1" name="clear sky"/>
                            <visibility/>
                            <precipitation value="0.2" mode="rain" unit="1h"/>
                            <weather number="500" value="lluvia ligera" icon="10d"/>
                            <lastupdate value="2016-04-03T15:35:32"/>
                            </current>
                        """> ///"""

La llamada a la API se realiza usando FSharp.Data.Http

	let res = Http.RequestString("http://api.openweathermap.org/data/2.5/weather", 
                        httpMethod = "GET", query=["q", city; "mode", "xml"; "units", "metric"; "lang", "sp"; "appid", api_key],
                        headers = [Accept HttpContentTypes.Xml])
        let xml = Result.Parse(res)
	</current>

