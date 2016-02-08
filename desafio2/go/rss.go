package main

import ("encoding/xml"; "html/template"; "log")

// based on http://siongui.github.io/2015/03/03/go-parse-web-feed-rss-atom/
// with bugs fixed :)

type Rss2 struct {
	XMLName		xml.Name	`xml:"rss"`
	Version		string		`xml:"version,attr"`
	// Required
	Title		string		`xml:"channel>title"`
	Link		string		`xml:"channel>link"`
	Description	string		`xml:"channel>description"`
	// Optional
	PubDate		string		`xml:"channel>pubDate"`
	ItemList	[]Item		`xml:"channel>item"`
}

type Item struct {
	// Required
	Title		string		`xml:"title"`
	Link		string		`xml:"link"`
	Description	template.HTML	`xml:"description"`
	// Optional
	Content		template.HTML	`xml:"encoded"`
	PubDate		string		`xml:"pubDate"`
	Comments	string		`xml:"comments"`
}


type Atom1 struct {
	XMLName		xml.Name	`xml:"http://www.w3.org/2005/Atom feed"`
	Title		string		`xml:"title"`
	Subtitle	string		`xml:"subtitle"`
	Id		    string		`xml:"id"`
	Updated		string		`xml:"updated"`
	Rights		string		`xml:"rights"`
	LinkList	[]Link		`xml:"link"`
	Author		Author		`xml:"author"`
	EntryList	[]Entry		`xml:"entry"`
}

type Link struct {
	Href		string		`xml:"href,attr"`
}

type Author struct {
	Name		string		`xml:"name"`
	Email		string		`xml:"email"`
}

type Entry struct {
	Title		string		`xml:"title"`
	Link		Link		`xml:"link"`
	Id		string		`xml:"id"`
	Updated		string		`xml:"updated"`
	Summary		string		`xml:"summary"`
	Content		string		`xml:"content"`
	Author		Author		`xml:"author"`
}


func atom1ToRss2(a Atom1) Rss2 {
	r := Rss2{
		Title: a.Title,
		Link: a.LinkList[0].Href,
		Description: a.Subtitle,
		PubDate: a.Updated,
	}
	r.ItemList = make([]Item, len(a.EntryList))
	for i, entry := range a.EntryList {
		r.ItemList[i].Title = entry.Title
		r.ItemList[i].Link = entry.Link.Href
		r.ItemList[i].PubDate = entry.Updated
		r.ItemList[i].Description = template.HTML(entry.Summary)
		if entry.Content == "" {
			r.ItemList[i].Content = template.HTML(entry.Summary)
		} else {
			r.ItemList[i].Content = template.HTML(entry.Content)
		}
	}
	return r
}


const atomErrStr = "expected element type <rss> but have <feed>"

func parseAtom(content []byte) (Rss2, bool){
	a := Atom1{}
	err := xml.Unmarshal(content, &a)
	if err != nil {
		log.Println(err)
		return Rss2{}, false
	}
	log.Println("updated: "+a.Updated)
	return atom1ToRss2(a), true
}

func ParseFeedContent(content []byte) (Rss2, bool) {
	v := Rss2{}
	err := xml.Unmarshal(content, &v)
	if err != nil {
		if err.Error() == atomErrStr {
			// try Atom 1.0
			return parseAtom(content)
		}
		log.Println(err)
		return v, false
	}

	if v.Version == "2.0" {
		// RSS 2.0
		for i, _ := range v.ItemList {
			if v.ItemList[i].Content != "" {
				v.ItemList[i].Description = v.ItemList[i].Content
			}
		}
		return v, true
	}

	log.Println("not RSS 2.0")
	return v, false
}
