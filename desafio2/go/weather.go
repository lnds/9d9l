package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"sort"
	"time"
)

type WeatherReport struct {
	error      string
	city       string
	temp       float32
	max        float32
	min        float32
	conditions string
}

const time_report_message = "tiempo ocupado para generar el reporte: "
const report_format = "%-30.30s max:%5.1f  min:%5.1f   actual: %5.1f %s"
const no_cities_provided_message = "debe ingresar una lista de ciudades"

func url(city string, api_key string) string {
	return "http://api.openweathermap.org/data/2.5/weather?q=" + city + "&mode=xml&units=metric&appid=" + api_key + "&lang=sp"
}

func main() {

	if len(os.Args) == 1 || (len(os.Args) == 2 && os.Args[1] == "-p") {
		fmt.Println(no_cities_provided_message)
		os.Exit(0)
	}

	api_key := os.Getenv("WEATHER_API_KEY")

	start := time.Now()

	reports := make([]WeatherReport, 0)

	if os.Args[1] == "-p" {
		ch := make(chan WeatherReport)
		for _, city := range os.Args[2:] {
			go fetch(city, api_key, ch)
		}
		for range os.Args[2:] {
			rep := <-ch
			reports = append(reports, WeatherReport{rep.error, rep.city, rep.temp, rep.max, rep.min, rep.conditions})
		}
	} else {
		for _, city := range os.Args[1:] {
			rep := seqFetch(city, api_key)
			reports = append(reports, WeatherReport{rep.error, rep.city, rep.temp, rep.max, rep.min, rep.conditions})
		}
	}

	sort.Sort(ByTemp(reports))
	for _, r := range reports {
		if r.error == "" {
			fmt.Printf(report_format, r.city, r.max, r.min, r.temp, r.conditions)
		} else {
			fmt.Printf("%s Error: %s\n", r.city, r.error)
		}
		fmt.Println()
	}
	duration := time.Since(start)
	fmt.Printf("%s: %02.0f:%02.0f:%02.3f\n", time_report_message, duration.Hours(), duration.Minutes(), duration.Seconds())
}

type ByTemp []WeatherReport

func (a ByTemp) Len() int           { return len(a) }
func (a ByTemp) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a ByTemp) Less(i, j int) bool { return a[i].max > a[j].max }

const max_tries = 10
const thread_sleep = 100

func fetch(city string, api_key string, ch chan<- WeatherReport) {
	url := url(city, api_key)
	var resp *http.Response
	var err error
	resp, err = http.Get(url)
	for i := 0; i < max_tries && err == nil; i++ {
		time.Sleep(thread_sleep)
		resp, err = http.Get(url)
	}
	if err != nil {
		ch <- WeatherReport{"error leyendo xml ", city, 0.0, 0.0, 0.0, ""}
		return
	}

	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return
	}
	if weather, success := ParseCurrentWeather(body); success {
		ch <- WeatherReport{"", weather.City.Name,
			weather.Temperature.Value,
			weather.Temperature.Max, weather.Temperature.Min,
			weather.Weather.Value}
	}
}

func seqFetch(city string, api_key string) WeatherReport {
	url := url(city, api_key)
	resp, err := http.Get(url)
	if err != nil {
		return WeatherReport{"error descargando url ", city, 0.0, 0.0, 0.0, ""}
	}

	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return WeatherReport{"error leyendo xml ", city, 0.0, 0.0, 0.0, ""}
	}

	weather, success := ParseCurrentWeather(body)
	if success {
		return WeatherReport{"", weather.City.Name, weather.Temperature.Value,
			weather.Temperature.Max, weather.Temperature.Min, weather.Weather.Value}
	}
	return WeatherReport{"error interpretando xml ", city, 0.0, 0.0, 0.0, ""}
}
