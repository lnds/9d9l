import Foundation

let apiKey = ProcessInfo.processInfo.environment["WEATHER_API_KEY"]
if (apiKey == nil) {
	print("debe definir variable de ambiente WEATHER_API_KEY")
	exit(-1)
}


let args = ProcessInfo.processInfo.arguments
let argc = ProcessInfo.processInfo.arguments.count

if argc == 1 || (argc == 2 && args[1] == "-p") {
	print("debe ingresar una lista de ciudades")
	exit(-1)
}
let par = args[1] == "-p"
let cities =  par ? args[2..<args.count] : args[1..<args.count]


let start = Date()

var reports = [ApiResult?]()
if par {
	for i in 1...cities.count {
		reports.append(nil)
	}
	let _ = DispatchQueue.global(qos:.userInitiated)
	DispatchQueue.concurrentPerform(iterations:cities.count, execute: {
		i in 
			let index = Int(i)+2 // cities start from 2
			let city = cities[index]
			let rep = callApi(city:city, apiKey:apiKey!)
			reports[Int(i)] = rep
	})
} else {
	for city in cities {
		reports.append(callApi(city:city, apiKey:apiKey!))
	}

}

// sort result
reports.sort(by:{
	switch $0! {
	case .Error(let city, let error):
		return false
	case .Weather(let city, let t0, let mx0, let mn0, let conditions):
		switch $1! {
		case .Error(let city, let error):
			return false
		case .Weather(let city, let t1, let mx1, let mn1, let conditions):
			return t1 < t0
		}
	}	
})


let end = Date()
let timeInterval = end.timeIntervalSince(start)

// show reports
for rep in reports {
	switch rep! {
	case .Error(let city, let error):
		print("\(city) Error: \(error)")
	case .Weather(let city, let temp, let max, let min, let conditions):
		print(city.padding(toLength:30, withPad:" ", startingAt: 0),String(format:"max:%5.1f  min:%5.1f   actual: %5.1f", temp, max, min),conditions)
	}
}
let hours = (Int(timeInterval) / 3600)
let mins =  (Int(timeInterval) / 3600) / 60
let secs = (timeInterval.truncatingRemainder(dividingBy:3600.0)).truncatingRemainder(dividingBy:  60.0)
print(String(format:"tiempo ocupado para generar el reporte: %02d:%02d:%05.2f", hours, mins, secs))