package main

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"strings"
	"time"
	"unicode"
)

func shuffle(a []int) []int {
	rand.Seed(time.Now().UnixNano())
	rand.Shuffle(len(a), func(i, j int) {
		a[i], a[j] = a[j], a[i]
	})
	return a
}

func reglas(tam int) {
	fmt.Printf(
		`
Bienvenido a Toque y Fama.
==========================
En este juego debes tratar de adivinar una secuencia de %[1]d dígitos generadas por el programa.
Para esto ingresas %[1]d dígitos distintos con el fin de adivinar la secuencia.
Si has adivinado correctamente la posición de un dígito se produce una Fama.
Si has adivinado uno de los dígitos de la secuencia, pero en una posición distinta se trata de un Toque.
Ejemplo: Si la secuencia es secuencia: [8, 0, 6, 1, 3] e ingresas 40863, entonces en pantalla aparecerá:
tu ingresaste [4 0 8 6 3]
resultado: 2 Toques 2 Famas


`, tam)
}

var reader = bufio.NewReader(os.Stdin)

func entrada(tam int) string {
	fmt.Printf("Ingresa una secuencia de %d dígitos distintos (o escribe salir):\n", tam)
	entrada, _ := reader.ReadString('\n')
	return strings.TrimSpace(entrada)
}

func validar(tam int, accion string) (num []int, ok bool) {
	for i, c := range accion {
		if !unicode.IsDigit(c) || i >= tam  {
			return 
		} 
		digit := int(c - '0')
		for _, d := range num {
			if digit == d {
				return
			}
		}
		num = append(num, digit)
	}
	ok = len(num) == tam
	return 
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
	sec := shuffle([]int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9})[0:tam]
	reglas(tam)
	intentos := 0
	for {
		intentos++
		accion := entrada(tam)
		if accion == "salir" {
			fmt.Println("\ngracias por jugar, adios.")
			break
		}
		if num, ok := validar(tam, accion); !ok {
			fmt.Println("error!\n")
		} else {
			fmt.Println("ingresaste: ", num)
			toques, famas := comparar(num, sec)
			fmt.Printf("resultado: %d Toques, %d Famas\n\n", toques, famas)
			if famas == tam {
				fmt.Printf("Ganaste! Acertaste al intento %d! La secuencia era %v.\n", intentos, sec)
				break
			}
		}
	}
}
