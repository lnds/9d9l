package main

import ("fmt"; "io/ioutil"; "net/http"; "os"; "time"; "sort"; "strings"; "html/template")
import "golang.org/x/net/html"

const (
	LINE_SIZE = 140
	MAX_LINES = 3
)

type ReadOperation struct {
	error string
	url   string
	rss   *Rss2
}

type ReadItem struct {
	error string
	url   string
	pub   time.Time
	item  *Item
}

func main() {
	fmt.Println("Concurrent news reader")

	start := time.Now()
	ch := make(chan ReadOperation)
	for _, url := range os.Args[1:] {
		go fetch(url, ch)
	}

	news := make([]ReadItem, 0)
	for range os.Args[1:] {
		rop := <- ch
		if rop.rss == nil {
			news = append(news, ReadItem{rop.error, rop.url, time.Now(), nil})
		} else {
			for _, item := range rop.rss.ItemList {
				newItem := item
				news = append(news, ReadItem{"", rop.url, parseTime(newItem.PubDate), &newItem})
			}
		}
	}
	sort.Sort(ByDate(news))
	if len(news) > 10 {
		news = news[0:9]
	}
	for _, ri := range news {
		if ri.item == nil {
			fmt.Println("Fuente: "+ri.url)
			fmt.Println("Error: "+ri.error)
		} else {
			fmt.Println("Título: "+ri.item.Title)
			fmt.Println("Fuente: "+ri.url)
			fmt.Printf("Fecha: %d-%02d-%02d %02d:%02d:%02d\n", 
				ri.pub.Year(), ri.pub.Month(), ri.pub.Day(), ri.pub.Hour(), ri.pub.Minute(), ri.pub.Second())
			fmt.Println(parseHtmlFragment(ri.item.Content))
			fmt.Println()
		}
	}
	duration := time.Since(start)
	fmt.Printf("Tiempo ocupado en descargar las noticias: %02.0f:%02.0f:%02.3f\n", duration.Hours(), duration.Minutes(), duration.Seconds())
}

func parseHtmlFragment(htmlFragment template.HTML) string {
	tokenizer := html.NewTokenizer(strings.NewReader(string(htmlFragment)))
	result := ""
	for {
		tok := tokenizer.Next()
		switch tok {
		case html.ErrorToken:
			lines := strings.Split(result, "\n")
			if len(lines) > MAX_LINES {
				lines = lines[0:MAX_LINES]
			}
			for i, s := range lines {
				if len(s) > LINE_SIZE {
					s = s[0:LINE_SIZE-3]+"..."
				}
				lines[i] = s
			}
			return strings.Join(lines, "\n")
		case html.EndTagToken:
				t :=tokenizer.Token()
				if t.Data == "p" {
					result = result + "\n"
				}
		case html.TextToken:
			str := strings.TrimSpace(string(tokenizer.Text()))
			if len(str) > 0 {
				result = result + str + " "
			}

		}
	}
}

func parseTime(timeStr string) time.Time {
	t, err := time.Parse(time.RFC3339, timeStr)
	if err == nil {
		return t
	} 
	t, err = time.Parse(time.RFC822, timeStr)
	if err == nil {
		return t
	} 
	t, err = time.Parse(time.RFC1123, timeStr)
	if err == nil {
		return t
	}
	return time.Now()
}

type ByDate []ReadItem

func (a ByDate) Len() int { return len(a) }
func (a ByDate) Swap(i, j int) { a[i], a[j] = a[j], a[i]}
func (a ByDate) Less(i, j int) bool {
	if a[i].item == nil || a[j].item == nil {
		return true
	} else {
		return a[i].pub.After(a[j].pub)
	} 
}

func fetch(url string, ch chan <- ReadOperation) {
	resp, err := http.Get(url)
	if err != nil {
		ch <- ReadOperation { "error fetching url ", url, nil}
		return
	}

	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		ch <- ReadOperation { "error reading xml ", url, nil }
	} else {
		rss, success := ParseFeedContent(body)
		if success {
			ch <- ReadOperation { "", url, &rss }
		} else {
			ch <- ReadOperation { "error parsing xml ", url, nil}
		}
	}
}
