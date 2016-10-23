import Foundation

enum ApiResult {
	case Error(String, String) // city, error
	case Weather(String,Double,Double,Double,String)  // city, temp,  max, min,conditions	
}

func httpGet(city:String, url:String) -> ApiResult {

	if let myURL = URL(string: url) {
		for _ in 1...10 {
			do {
				let xmlDoc = try XMLDocument(contentsOf: myURL, options:0)
				if let root = xmlDoc.rootElement() {
					let cityName = root.elements(forName:"city")[0].attribute(forName:"name")!.stringValue!
					let temp = (root.elements(forName:"temperature")[0].attribute(forName:"value")!.stringValue! as NSString).doubleValue
					let max  = (root.elements(forName:"temperature")[0].attribute(forName:"max")!.stringValue! as NSString).doubleValue
					let min  = (root.elements(forName:"temperature")[0].attribute(forName:"min")!.stringValue! as NSString).doubleValue
					let weatherConds = root.elements(forName:"weather")[0].attribute(forName:"value")!.stringValue!
					return ApiResult.Weather(cityName, temp, max, min, weatherConds)
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
	let url = makeUrl(city:city, apiKey:apiKey)
	return httpGet(city:city, url:url)
}
