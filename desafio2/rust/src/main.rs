extern crate getopts;
extern crate treexml;
use getopts::Options;
use std::thread;
use std::sync::mpsc;
use std::env;
use treexml::Document;
use std::cmp::Ordering;
use cabot::{RequestBuilder, Client};

enum ApiResult {
    Error{city: String, error: String},
    WeatherReport{city: String, temp: f32, max:f32, min:f32, conditions: String}
}

fn api_call(city:&String, api_key:&String) -> ApiResult {
    let url = format!("http://api.openweathermap.org/data/2.5/weather?q={}&mode=xml&units=metric&appid={}&lang=sp", city, api_key);
    let request = RequestBuilder::new(&url).build().unwrap();
    let client = Client::new();
    for _ in 1..10 {
        match client.execute(&request) { 
            Err(_) => {
                std::thread::sleep(std::time::Duration::from_millis(100)) 

            },
            Ok(res) => {
                let body = res.body_as_string().unwrap();
                match Document::parse(body.as_bytes()) {
                    Err(_) => {
                        std::thread::sleep(std::time::Duration::from_millis(100)) 
                    },
                    Ok(doc) => {
                        let root = doc.root.unwrap();
                        let temp = root.find_child(|tag| tag.name == "temperature").unwrap().clone();
                        let weather = root.find_child(|tag| tag.name == "weather").unwrap().clone();

                        let min_temp: f32= temp.attributes["min"].parse().unwrap();
                        let max_temp: f32= temp.attributes["max"].parse().unwrap();
                        let cur_temp: f32= temp.attributes["value"].parse().unwrap();

                        return ApiResult::WeatherReport{city: city.clone(), 
                            temp: cur_temp, max: max_temp, min:min_temp, 
                            conditions: weather.attributes["value"].clone() }
                    }
                }
            }
        }
    }
    ApiResult::Error{city: city.clone(), error: "error invocando api".to_string()}
}

fn par_fetch(cities:&Vec<String>, api_key:String) -> Vec<ApiResult> {
    let (tx, rx) = mpsc::channel();

    for city in cities {
        let tx = tx.clone();
        let city = city.clone();
        let api_key = api_key.clone();
        thread::spawn(move || {
            let report = api_call(&city, &api_key);
            tx.send(report).unwrap();
        });
    }
    let mut reports : Vec<ApiResult> = Vec::with_capacity(cities.len());
    for _ in cities {
        let rep = rx.recv().unwrap();
        reports.push(rep);
    }
   reports 
}

fn seq_fetch(cities:&Vec<String>, api_key:String) -> Vec<ApiResult> {
    let mut reports : Vec<ApiResult> = Vec::with_capacity(cities.len());
    for city in cities {
        reports.push(api_call(city, &api_key));
    }
    reports 
}

fn comp_api_result(a:&ApiResult, b:&ApiResult) -> Ordering {
    match a {
        &ApiResult::Error{city:_, error:_} => Ordering::Less,
        &ApiResult::WeatherReport{city:_, temp:ta, max:_, min:_, conditions:_} => 
            match b {
                &ApiResult::Error{city:_, error:_} => Ordering::Less,
                &ApiResult::WeatherReport{city:_, temp:tb, max:_, min:_, conditions:_} => 
                    if ta > tb { Ordering::Less } else { Ordering::Greater }
            }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut opts = Options::new();
    opts.optflag("p", "par", "ejecuta la busqueda en paralelo");
    let matches = match opts.parse(&args[1..]) {
    	Ok(m) => { m }
    	Err(f) => { panic!(f.to_string()) }
    };
    let par = matches.opt_present("p");
    let cities = matches.free;
    if cities.is_empty() {
    	println!("debe ingresar una lista de ciudades");
    	return;
    }
    let api_key = env::var("WEATHER_API_KEY").expect("debe definir WEATHER_API_KEY");

    let t0 = std::time::Instant::now();
    let mut result = if par { par_fetch(&cities, api_key) } else { seq_fetch(&cities, api_key) };

    result.sort_by(|a,b| comp_api_result(&a,&b));
    for report in result {
        match report {
            ApiResult::Error{city:c, error:e} => { println!("{:?} Error: {:?}", e, c) }
            ApiResult::WeatherReport{city, temp, max, min, conditions} =>  { println!("{:<30} max:{:5.1}  min:{:5.1}   actual: {:5.1} {}", city, temp, max, min, conditions) }
        }
    }
    let dur = t0.elapsed();
    let secs = dur.as_secs();
    let nanos = dur.subsec_nanos();
    let hours = secs / 3600;
    let mins  = (secs % 3600) / 60;
    let sec  = (secs % 3600) % 60;
    let frac  = nanos/1000000;
    println!("tiempo ocupado para generar el reporte: {} = {}:{}:{}.{}", secs, hours, mins, sec, frac);
    
}
