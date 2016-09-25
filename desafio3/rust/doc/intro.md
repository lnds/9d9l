# Ordenar Vector en Rust

## Archivos

Abrir un archivo de entrada:

    let entrada = match File::open(&args[1]) {
        Err(e) => panic!("No pudo abrir archivo {}, causa: {}", args[1], e.description()),
        Ok(file) => BufReader::new(file)
    };

En este caso usamos File::open() lo que devuelve un enum, indicando error (Err) o éxito (Ok). Si tenemos éxito en abrir el archivo, devolvemos un BufReader que es un objeto que nos permitirá leer linea por linea el archivo.

De forma similar, el archivo de salida se crea de este modo:

    let mut salida = match File::create(&args[2]) {
        Err(e) => panic!("No pudo crear archivo {}, causa: {}", args[2], e.description()),
        Ok(file) => LineWriter::new(file)
    };

Notar que devolvemos un objeto LineWriter que permite escribir linea por linea el resultado usando un buffer que detecta los fines de linea para ejecutar un flush a disco del archivo.


La lectura de todas las lineas del archivo se hace así:

    for (num_linea, linea) in entrada.lines().enumerate() {

    }

Notar que en el for usamos el método enumerate() que nos permite asignarle un número a cada linea leida.


## Slices y Arreglos de Bytes

Tratamos cada linea como un arreglo de bytes (nuestro archivo de entrada está codificado en ASCII).



    for (num_linea, linea) in entrada.lines().enumerate() {
        let buf = &linea.unwrap().into_bytes();
        if buf.len() != LARGO_LINEA {
            println!("Error en el largo de la linea {}", num_linea);
        } else {
            let vector = procesar_linea(buf);
            writeln!(salida, "{}", str::from_utf8(&vector).unwrap()).unwrap();
        }
    }

Cuando hacemos linea.unwrap() ignoramos cualquier error en la lectura de una linea (de lo contrario tendríamos que escribir un match).
La linea se convierte en bytes usando el método .into_bytes().


La función procesar_linea() es muy simple, pero hay que tener cuidado al analizarla:

    fn procesar_linea(buf: &Vec<u8>) -> [u8; TAM_SALIDA] {
        let mut res : [u8; TAM_SALIDA] = [' ' as u8; TAM_SALIDA];
        res[0..POS_VECTOR].clone_from_slice(&buf[0..POS_VECTOR]);
        ordenar_vector(&buf[POS_VECTOR..], &mut res[POS_VECTOR..]); 
        return res;
    }



Esta función recibe un vector de bytes y retorna un vector de TAM_SALIDA bytes (esta es una constante definida en las primeras líneas de código).

La variable res contiene el vector de salida (relleno con espacios en blanco).

La siguiente instrucción:

        res[0..POS_VECTOR].clone_from_slice(&buf[0..POS_VECTOR]);

Copia en un slice (res[0..POS_VECTOR]) el valor de otro slice (&buf[0..POS_VECTOR]). Hay que notar que al hacer buf[0..POS_VECTOR] construimos un slice del tamaño requerido. Se coloca el & para psarlo por referencia a clone_from_slice().

Después viene la función ordenar_vector() que recibe el vector con los periodos y una referencia mutable a res. Esto quiere decir que la función ordenar_vector() modificará el contenido de la variable res.

Hay que fijarse que la variable salida se indexa a partir de 0, pero al invocarla pasamos la variable result a partir de la posición POS_VECTOR.

Esto es crucial y se debe entender bien, es la forma que tenemos en Rust de trabajar sobre áreas de memoria compartida. En C ó C++ habríamos usado un puntero.

## El ordenamiento

El código del ordenamiento es el siguiente:

   
    fn ordenar_vector(vector:&[u8],  result:&mut [u8]) {
        let mut n = 0;
        let cero = ['0' as u8; TAM_PERIODO];
        for p in vector.chunks(TAM_PERIODO) {

            if p == cero { continue; }

            let mut i = 0;
            let mut q = 1;
            while i < n && p < &result[q..q+TAM_PERIODO] { i += 1; q += TAM_PERIODO; } // busca si p está en el arreglo

            if i < n && p == &result[q..q+TAM_PERIODO] { continue; } // si ya existe lo ignora

            // inserta p en el arreglo
            if i == n {
                if n < ELEMENTOS_VECTOR {
                    let q = n * TAM_PERIODO+1;
                    result[q..q+TAM_PERIODO].clone_from_slice(p);
                }
            } else {
                for j in (i+1..ELEMENTOS_VECTOR).rev() {
                    let q = j*TAM_PERIODO+1;
                    unsafe {
                        ptr::copy_nonoverlapping(&mut result[q-TAM_PERIODO], &mut result[q], TAM_PERIODO)
                    }
                }
                let q = i*TAM_PERIODO+1;
                result[q..q+TAM_PERIODO].clone_from_slice(p);
            }
            n += 1;
            // si excedimos el tamaño del vector salimos
            if n > ELEMENTOS_VECTOR { break; }
        }

        // retorna el resultado
        if n == 0 {
            result[0] = 'N' as u8;
        } else if n > ELEMENTOS_VECTOR {
            result[0] = 'S' as u8;
            for j in 1..result.len() {
                result[j] = ' ' as u8
            }
        } else {
            result[0] = 'D' as u8;
        }
    }


Acá la función chunks() divide el arreglo vector en bloeque de tamaño TAM_PERIODO, los que pueden ser comparados con lo contenido en result.
Por eso que tenemos comparaciones como:

    p == cero
    p < &result[i]
    p == &result[i]

En C tendriamos que llamar a una función como strncmp() o similar para comparar ambos slices. Notar que chunks() retorna un iterador a referencias de tamaños TAM_PERIODO.

La estructura de este loop es compleja pues busca terminar lo antes posible cuando un vector excede el tamaño. Del mismo modo, evita los duplicados y realiza el incremento de la variable n sólo una vez. 

Esta versión modifica el arreglo directamente, por eso se recibe como una referencia mutable (&mut).

Dado que trabajamos sobre el mismo arreglo de salida en un momento tenemos que acceder y mover los bytes dentro del mismo arreglo. Esto se hace usando código unsafe:

    unsafe {
        ptr::copy_nonoverlapping(&mut result[q-TAM_PERIODO], &mut result[q], TAM_PERIODO)
    }

Esto es como la función memmove() de C.

Problema propuesto: se puede optimizar más este loop? Hay una manera más eficiente de lograr lo mismo?