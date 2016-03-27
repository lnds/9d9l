# Weather en Rust

## Cliente HTTP

No hay una manera estándar de hacer un request en Rust por ahora.
Por esto que usamos Hyper.
A la fecha (marzo de 2016), esto requiere configuraciones especiales para Mac OSX que están descritas en el archivo README.md.

## Ejecución en paralelo

La funcion par_fetch() utiliza channels al estilo de la implementación en Go.
En esencia es:

	let (tx, rx) = mpsc::channel();
	for city in cities {
        let tx = tx.clone();
        let city = city.clone();
        thread::spawn(move || {
            let report = api_call(&city, &api_key);
            tx.send(report).unwrap();
        });
    }
    for _ in cities {
        let rep = rx.recv().unwrap();
        reports.push(rep);
    }

## Parsing XML

No hay soporte en las bibliotecas estándar de Rust, es por esto que usamos el crate treexml, que es bastante simple para la aplicación.