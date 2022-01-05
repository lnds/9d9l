package main

import "encoding/xml"

// based on http://siongui.github.io/2015/03/03/go-parse-web-feed-rss-atom/
// with bugs fixed :)

type City struct {
	Name    string `xml:"name,attr"`
	Country string `xml:"country"`
}

type Weather struct {
	Value string `xml:"value,attr"`
}

type Temperature struct {
	Value float32 `xml:"value,attr"`
	Min   float32 `xml:"min,attr"`
	Max   float32 `xml:"max,attr"`
}

type Current struct {
	XMLName     xml.Name    `xml:"current"`
	City        City        `xml:"city"`
	Temperature Temperature `xml:"temperature"`
	Weather     Weather     `xml:"weather"`
}

func ParseCurrentWeather(content []byte) (Current, bool) {
	v := Current{}
	err := xml.Unmarshal(content, &v)
	if err != nil {
		return v, false
	}

	return v, true
}
