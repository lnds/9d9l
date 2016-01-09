import java.util.*

fun shuffle(arr: IntArray) : IntArray {
    val rg : Random = Random()
    for (i in arr.indices) {
        val j = rg.nextInt(i+1)
        val tmp = arr[i]
        arr[i] = arr[j]
        arr[j] = tmp
    }
    return arr
}

fun mostrarReglas(tam:Int) {
    println("""
Bienvenido a Toque y Fama.
==========================

En este juego debes tratar de adivinar una secuencia de ${tam} dígitos generadas por el programa.
Para esto ingresas ${tam} dígitos distintos con el fin de adivinar la secuencia.
Si has adivinado correctamente la posición de un dígito se produce una Fama.
Si has adivinado uno de los dígitos de la secuencia, pero en una posición distinta se trata de un Toque.

Ejemplo: Si la secuencia es secuencia: [8, 0, 6, 1, 3] e ingresas 40863, entonces en pantalla aparecerá:
tu ingresaste [4, 0, 8, 6, 3]
resultado: 2 Toques 2 Famas

    """)
}

fun validar(tam:Int, accion:String?) : IntArray? {
    if (accion == null) return null
    var num = emptyArray<Int>()
    for (it in accion.withIndex()) {
        if (!it.value.isDigit())
            return null
        if (it.index >= tam)
            return null
        else {
            val digit = it.value.toInt() - '0'.toInt()
            if (digit in num) return null
            num = num.plus(digit)
        }
    }
    return num.toIntArray()
}

fun comparar(num: IntArray, sec: IntArray) : Pair<Int,Int> {
    var toques = 0
    var famas = 0
    for ((i,n) in num.withIndex())
        for ((j,m) in sec.withIndex())
            if (n == m) {
                if (i == j) famas++
                else toques++
            }
    return Pair(toques, famas)
}

fun main(args: Array<String>) {
    val tam = 5
    mostrarReglas(tam)
    val sec = shuffle(intArrayOf(0,1,2,3,4,5,6,7,8,9)).take(tam).toIntArray()
    var intentos = 0
    while (true) {
        intentos++
        println ("Ingresa una secuencia de ${tam} dígitos distintos (o escribe salir):")
        val accion = readLine()
        if (accion == "salir") break
        else {
            val num = validar(tam, accion)
            if (num == null)
                println("error!\n")
            else {
                println("ingresaste: [${num.joinToString()}]")
                val (toques, famas) = comparar(num, sec)
                println("resultado: ${toques} Toques, ${famas} Famas\n")
                if (famas == tam) {
                    println("Ganaste! Acertaste al intento ${intentos}! La secuencia era [${sec.joinToString()}].\n")
                    break
                }
            }
        }
    }
}