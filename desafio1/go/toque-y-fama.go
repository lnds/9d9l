package main

import ("bufio";"fmt";"math/rand";"os";"strings";"time";"unicode")

func shuffle(a []int) []int {
	rand.Seed(time.Now().UnixNano())
	for i := range a {
		j := rand.Intn(i+1)
		a[i], a[j] = a[j], a[i]
	}
	return a
}

func mostrar_reglas(tam int) {
	fmt.Printf(
		"Bienvenido a Toque y Fama.\n"+
		"==========================\n\n"+
		"En este juego debes tratar de adivinar una secuencia de %[1]d dígitos generadas por el programa.\n"+
		"Para esto ingresas %[1]d dígitos distintos con el fin de adivinar la secuencia.\n"+
		"Si has adivinado correctamente la posición de un dígito se produce una Fama.\n"+
		"Si has adivinado uno de los dígitos de la secuencia, pero en una posición distinta se trata de un Toque.\n\n"+
		"Ejemplo: Si la secuencia es secuencia: [8, 0, 6, 1, 3] e ingresas 40863, entonces en pantalla aparecerá:\n"+
		"tu ingresaste [4, 0, 8, 6, 3]\n"+
		"resultado: 2 Toques 2 Famas\n\n\n", tam)
}

func leer_entrada(tam int) string {
	reader := bufio.NewReader(os.Stdin)
	fmt.Printf("Ingresa una secuencia de %d dígitos distintos (o escribe salir):\n", tam)
	entrada, _ := reader.ReadString('\n')
	return strings.TrimSpace(entrada)
}

func validar(tam int, accion string) []int {
	num := make([]int, 0, tam)
	for i, c := range accion {
		if (!unicode.IsDigit(c)) {
			return nil
		} else if (i >= tam) {
			return nil
		} else {
			digit := int (c - '0')
			for _, d := range num {
				if digit == d { return nil }
			}
			num = num[0:i+1]
			num[i] = digit
		}
	}
	if len(num) != tam { return nil }
	return num
}

func comparar(num, sec []int) (toques int, famas int) {
	for i, n := range num {
		for j, m := range sec {
			if n == m {
				if i == j { 
					famas++ 
				} else { 
					toques++ 
				}
			}
		}
	}
	return
}

func main() {
	const tam = 5
	sec := shuffle([]int{0,1,2,3,4,5,6,7,8,9})[0:tam]
	mostrar_reglas(tam)
	for intentos,salir := 0, false; !salir; intentos++ {
		accion := leer_entrada(tam)
		if accion == "salir" {
			fmt.Println("\ngracias por jugar, adios.")
			salir = true
		} else {
			num := validar(tam, accion)
			if num == nil {
				fmt.Println("error!\n")
			} else {
				fmt.Println("ingresaste: ", num)
				toques, famas := comparar(num, sec)
				fmt.Printf("resultado: %d Toques, %d Famas\n\n", toques, famas)
				if famas == tam {
					fmt.Printf("Ganaste! Acertaste al intento %d! La secuencia era %v.\n", intentos, sec)
					salir = true
				}
			}
		}
	}
}