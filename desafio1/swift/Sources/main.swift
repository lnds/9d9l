import FisherYates

func validar(tam:Int, acc:String!) -> [Int]? {
	var num = [Int]()
	let chars = Array(acc.characters)
	for (i,c) in chars.enumerated() {
		if i >= tam {
			return nil
		} else if c < "0" || c > "9" {
			return nil
		} else {
			let digito = Int(String(c))!
			if num.contains(digito) {
				return nil
			}
			num.append(Int(String(c))!)
		}
	}
	if num.count != tam { return nil }
	return num
}

func comparar(num:[Int], sec:[Int]) -> (toques:Int, famas:Int) {
	var toques = 0
	var famas = 0
	for (i, n) in num.enumerated() {
		for (j, m) in sec.enumerated() {
			if n == m {
				if i == j { famas += 1 } 
				else { 	toques += 1 }
			}
		}
	}
	return (toques, famas)
}

let tam = 5

var sec = Array([0,1,2,3,4,5,6,7,8,9].shuffled()[0..<tam])

print (" Bienvenido a Toque y Fama.\n",
	   "==========================\n\n",
		"En este juego debes tratar de adivinar una secuencia de \(tam) dígitos generadas por el programa.\n",
		"Para esto ingresas \(tam) dígitos distintos con el fin de adivinar la secuencia.\n",
		"Si has adivinado correctamente la posición de un dígito se produce una Fama.\n",
		"Si has adivinado uno de los dígitos de la secuencia, pero en una posición distinta se trata de un Toque.\n\n",
		"Ejemplo: Si la secuencia es secuencia: [8, 0, 6, 1, 3] e ingresas 40863, entonces en pantalla aparecerá:\n",
		"tu ingresaste [4, 0, 8, 6, 3]\n",
		"resultado: 2 Toques 2 Famas\n\n")

var intentos = 0
while true {
	intentos += 1
	print("Ingresa una secuencia de \(tam) dígitos distintos (o escribe salir):")
	let accion = readLine(strippingNewline:true)
	if accion == "salir" {
		break
	} else {
		let num = validar(tam:tam, acc:accion!)
		if num == nil {
			print("error!\n")
		} else { 
			print("ingresaste: ", num!)
			let (toques, famas) = comparar(num: num!, sec:sec)
			print("resultado: \(toques) Toques, \(famas) Famas\n")
			if famas == tam {
				print("Ganaste! Acertaste al intento \(intentos)! La secuencia era \(sec).")
				break
			} 
		}
	}
}