object ToqueFama {

    def main(args: Array[String]) : Unit = {
        val tam = 5
        val sec = util.Random.shuffle((0 to 9).toList).take(tam).toArray
        println (sec.mkString(", "))
        mostrarReglas(tam)
        var salir = false
        var intentos = 0
        while (!salir) {
            intentos += 1
            println(s"Ingresa una secuencia de ${tam} dígitos distintos (o escribe salir):")
            val accion = scala.io.StdIn.readLine()
            if (accion == null || accion == "salir") {
                println("\ngracias por jugar, adios.")
                salir = true
            }
            else validar(tam, accion) match {
                    case None => println("error!\n")
                    case Some(num) =>
                        println(s"ingresaste: [${num.mkString(", ")}]")
                        val (toques, famas) = comparar(num, sec)
                        println(s"resultado: ${toques} Toques, ${famas} Famas")
                        if (famas == tam) {
                            println(s"Ganaste! Acertaste al intento ${intentos}! La secuencia era [${sec.mkString(", ")}]")
                            salir = true
                        }
                }
        }
    }

    def comparar(num:Array[Int], sec:Array[Int]) : (Int,Int) = {
        val tf = for (n,i) <- num.zipWithIndex; (m,j) <- sec.zipWithIndex if n == m yield if i == j then 'F' else 'T'
        (tf.filter(_ == 'T').length, tf.filter(_ == 'F').length)
    }

    def validar(tam:Int, accion:String) : Option[Array[Int]] = 
        if (accion.exists(!_.isDigit))
            None
        else {
            val num = accion.map(_.toInt - '0'.toInt).distinct
            if (num.length == tam && num.length == accion.length)
                Some(num.toArray)	
            else 
                None	
        }

    def mostrarReglas(tam:Int) : Unit = 
        println(s"""|Bienvenido a Toque y Fama.
                    |==========================\n
                    |En este juego debes tratar de adivinar una secuencia de ${tam} dígitos generadas por el programa.
                    |Para esto ingresas ${tam} dígitos distintos con el fin de adivinar la secuencia.
                    |Si has adivinado correctamente la posición de un dígito se produce una Fama.
                    |Si has adivinado uno de los dígitos de la secuencia, pero en una posición distinta se trata de un Toque.\n
                    |Ejemplo: Si la secuencia es secuencia: [8, 0, 6, 1, 3] e ingresas 40863, entonces en pantalla aparecerá:
                    |tu ingresaste [4, 0, 8, 6, 3]
                    |resultado: 2 Toques 2 Famas\n""".stripMargin)
    
}