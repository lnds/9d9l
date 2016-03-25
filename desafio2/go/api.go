package main

import ("encoding/xml";"fmt")

// based on http://siongui.github.io/2015/03/03/go-parse-web-feed-rss-atom/
// with bugs fixed :)

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


func ParseCurrentWeather(content []byte) (Current, bool) {
	v := Current{}
	err := xml.Unmarshal(content, &v)
	if err != nil {
		fmt.Println("ERROR!")
		fmt.Println(err)
		return v, false
	}

	return v, true
}
