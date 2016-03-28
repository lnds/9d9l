import Foundation

enum ApiResult {
	case Error(String, String) // city, error
	case Weather(String,Double,String)  // city, temp, conditions	
}

func httpGet(city:String, url:String) -> ApiResult {

	if let myURL = NSURL(string: url) {
		for _ in 1...10 {
			do {
				let xmlDoc = try NSXMLDocument(contentsOfURL: myURL, options:0)
				if let root = xmlDoc.rootElement() {
					let cityName = root.elementsForName("city")[0].attributeForName("name")!.stringValue!
					let temp = (root.elementsForName("temperature")[0].attributeForName("max")!.stringValue! as NSString).doubleValue
					let weatherConds = root.elementsForName("weather")[0].attributeForName("value")!.stringValue!
					return ApiResult.Weather(cityName, temp, weatherConds)
				} 
			} catch { /* DO NOTHING, and is good */ }
			usleep(100000)
		}
	} 
	return ApiResult.Error(city, "no pudo leer url")
}

func makeUrl(city: String, apiKey: String) -> String {
	return "http://api.openweathermap.org/data/2.5/weather?q=" + city + "&mode=xml&units=metric&appid=" + apiKey + "&lang=sp"
}

func callApi(city: String, apiKey: String) -> ApiResult {
	let url = makeUrl(city, apiKey:apiKey)
	return httpGet(city, url:url)
}
