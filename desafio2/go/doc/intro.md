# crdr en Go

Programar la ejecución concurrente de la descarga de urls fue lo más fácil en este proyecto.

El uso de channels simplifica mucho la sincronización.

El nucleo de la operación concurrente está en las lineas 28 a 31 de crdr.go

	ch := make(chan ReadOperation)
	for _, url := range os.Args[1:] {
		go fetch(url, ch)
	}

La función fetch corre concurrentemente por cada url.
Al terminar fetch escribe el resultado en el canal ch.

Fetch en esencia hace lo siguiente:

	func fetch(url string, ch chan <- ReadOperation) {
		resp, err := http.Get(url)
		...
			rss, success := ParseFeedContent(body)
			if success {
				ch <- ReadOperation { "", url, &rss }
			} else {
				ch <- ReadOperation { "error parsing xml ", url, nil}
			}

# Parsing de los feeds RSS y Atom

Para parsear los feeds usé un ejemplo encontrado en internet, pero que contenía algunos bugs, el resultado está en el archivo rss.go.

# Parsing de HTML

Para hacer parsing del HTML usamos un paquete experimental disponible en golang.org

	import "golang.org/x/net/html"

Para usar esto se debe instalar del siguiente modo:

	$ go get golang.org/x/net/html
