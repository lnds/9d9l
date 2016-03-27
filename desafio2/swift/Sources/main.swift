import Foundation

let apiKey = NSProcessInfo.processInfo().environment["WEATHER_API_KEY"]!

let args = Process.arguments
let argc = Process.argc

if argc == 1 || (argc == 2 && args[1] == "-p") {
	print("debe ingresar una lista de ciudades")
	exit(-1)
}
let par = args[1] == "-p"
let cities =  par ? args[2..<args.count] : args[1..<args.count]


let start = NSDate()

var reports = [ApiResult?]()
if par {
	for i in 1...cities.count {
		reports.append(nil)
	}
	let globalQueue = dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0)
	dispatch_apply(cities.count, globalQueue) {
		i in 
			let index = Int(i)+2 // cities start from 2
			let city = cities[index]
			let rep = callApi(city, apiKey:apiKey)
			reports[Int(i)] = rep
	}
} else {
	for city in cities {
		reports.append(callApi(city, apiKey:apiKey))
	}

}

// sort result
reports.sortInPlace({
	switch $0! {
	case .Error(let city, let error):
		return false
	case .Weather(let city, let t0, let conditions):
		switch $1! {
		case .Error(let city, let error):
			return false
		case .Weather(let city, let t1, let conditions):
			return t1 < t0
		}
	}	
})


let end = NSDate()
let timeInterval : Double = end.timeIntervalSinceDate(start)

// show reports
for rep in reports {
	switch rep! {
	case .Error(let city, let error):
		print("\(city) Error: \(error)")
	case .Weather(let city, let temp, let conditions):
		print(city.stringByPaddingToLength(30, withString:" ", startingAtIndex: 0),String(format:"%05.2f", temp),conditions)
	}
}
let hours = Int(timeInterval / 3600)
let mins = Int((timeInterval % 3600) / 60)
let secs = (timeInterval % 3600) %  60
print(String(format:"tiempo ocupado para generar el reporte: %02d:%02d:%05.2f", hours, mins, secs))