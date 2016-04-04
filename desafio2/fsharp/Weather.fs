
open System
open System.Threading
open FSharp.Data
open FSharp.Data.HttpRequestHeaders

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

type WeatherType = {city:string; max:decimal; min:decimal; temperature:decimal; conditions:string}  
type ErrorType = {city:string; error:string}
type ApiResulType = Weather of WeatherType | Error of ErrorType

let api_key = Environment.GetEnvironmentVariable("WEATHER_API_KEY")

let rec api_call_n city n =
    if n = 0 then
        Error{city=city; error="error invocando api"}
    else
        try
            let res = Http.RequestString("http://api.openweathermap.org/data/2.5/weather", 
                        httpMethod = "GET", query=["q", city; "mode", "xml"; "units", "metric"; "lang", "sp"; "appid", api_key],
                        headers = [Accept HttpContentTypes.Xml])
            let xml = Result.Parse(res)
            let name = xml.City.Name
            let temp = xml.Temperature.Value
            let max  = xml.Temperature.Max
            let min  = xml.Temperature.Min
            let cond = xml.Weather.Value
            Weather{city=name; max=max; min=min; temperature=temp; conditions=cond}
        with
            | _ -> Thread.Sleep(100)
                   api_call_n city (n - 1)

let api_call city = api_call_n city 10

let api_call_async city = async { return api_call_n city 10 }

// compara usado para ordenar
let weather_comp (a:ApiResulType) (b:ApiResulType) =
    match a with
    | Weather{city=_; temperature=ta; conditions=_ } ->
        match b with
        | Weather {city=c; temperature=tb; conditions=_ } -> int(tb - ta)
        | Error {city=_; error=_} -> -1
    | Error {city=_; error=_} -> -1

let print_reports reports =
    for rep in reports do
        match rep with
        | Weather {city=c; max=max; min=min; temperature=t; conditions=cond } -> printfn "%-30s max:%6.2f, min:%6.2f   actual: %6.2f %s" c max min t cond
        | Error {city=c; error=err} -> printf "%s %s" c err

let par_fetch cities = cities |> List.map api_call_async  |> Async.Parallel |> Async.RunSynchronously  |> List.ofArray

let seq_fetch cities = cities |> List.map api_call

[<EntryPoint>]
let main(argv: string[]) = 
    if argv.Length = 0 || (argv.Length = 1 && argv.[0] = "-p") then
        printfn "debe ingresar una lista de ciudades"
    else 
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let args = List.ofArray argv
        let reports = match args with
                        | par :: cities when par = "-p" -> par_fetch(cities)
                        | cities -> seq_fetch(cities) 
        reports |> List.sortWith weather_comp |> print_reports
        stopWatch.Stop()
        printfn "tiempo ocupado para generar el reporte: %d:%d:%d.%d" stopWatch.Elapsed.Hours stopWatch.Elapsed.Minutes stopWatch.Elapsed.Seconds  stopWatch.Elapsed.Milliseconds
    0