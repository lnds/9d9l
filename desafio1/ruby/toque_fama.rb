#!/usr/bin/env ruby

tamaño = 5
secuencia = (0..9).to_a.shuffle.first(tamaño)



puts <<FIN_TEXTO_BIENVENIDA
#{secuencia}
Bienvenido a Toque y Fama.
==========================

En este juego debes tratar de adivinar una secuencia de #{tamaño} dígitos generadas por el programa.
Para esto ingresas #{tamaño} dígitos distintos con el fin de adivinar la secuencia.
Si has adivinado correctamente la posición de un dígito se produce una Fama.
Si has adivinado uno de los dígitos de la secuencia, pero en una posición distinta se trata de un Toque.

Ejemplo: Si la secuencia es secuencia: [8, 0, 6, 1, 3] e ingresas 40863, entonces en pantalla aparecerá:
tu ingresaste [4, 0, 8, 6, 3]
resultado: 2 Toques 2 Famas
FIN_TEXTO_BIENVENIDA


intentos = 0
loop do
  intentos += 1

  puts "\nIngresa una secuencia de #{tamaño} dígitos distintos (o escribe salir):"
  if (linea = gets).nil? or linea.chomp! == 'salir'
    puts "\ngracias por jugar, adios."
    exit
  end

  numero = linea.gsub(/[^\d]/, '').split('').map(&:to_i).uniq
  if numero.length != tamaño or numero.join != linea
    puts "error!"
    next
  end

  puts "ingresaste: #{numero}"
  toques = numero.select { |n| secuencia.include? n }.count
  famas = numero.each_with_index.select { |n, i| secuencia[i] == n }.count
  toques -= famas

  puts "resultado: #{toques} Toques, #{famas} Famas"
  break if famas == tamaño
end

puts "Ganaste! Acertaste al intento #{intentos}! La secuencia era #{secuencia}"
