# Weather en Swift

## Concurrencia

Para ejecutar en modo concurrente usamos Grand Central Dispatch

	let globalQueue = dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0)
	dispatch_apply(cities.count, globalQueue) {
		i in 
			let index = Int(i)+2 // cities start from 2
			let city = cities[index]
			// print(city)
			let rep = callApi(city, apiKey:apiKey)
			reports[Int(i)] = rep
	}

Sugiero agregar un print(city) para verificar cómo se va ejecutando en paralelo y de forma asíncrona cada llamada.


## Cliente HTTP y Parsing XML

Fue lo que costó más averiguar, la invocación a la API se realiza así


	if let myURL = NSURL(string: url) {
		for _ in 1...10 {
			do {
				let xmlDoc = try NSXMLDocument(contentsOfURL: myURL, options:0)
				if let root = xmlDoc.rootElement() {
				// .. parsing

el for es para la cantidad de reintentos máximos.

Usamos un Enum muy similar a la solución en Rust, para retornar los resultados.

	enum ApiResult {
		case Error(String, String) // city, error
		case Weather(String,Double,String)  // city, temp, conditions	
	} 

## Notas

Imprimir el reporte del tiempo tiene esta forma: 

	print(city.stringByPaddingToLength(30, withString:" ", startingAtIndex: 0),String(format:"%05.2f", temp),conditions)

Traté de usar String(format:...) pero la opcion "%-30.30s" provoca un core dump! Sospecho que hay un bug en la implementación, es por eso que decidí usar stringByPaddingToLength().

