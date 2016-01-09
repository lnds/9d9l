module Main (main) where 

import Data.Char
import Data.List
import Control.Monad
import System.Random.Shuffle (shuffleM) 

mostrar_reglas n = do
    putStrLn $ "Bienvenido a Toque y Fama.\n\
	\==========================\n\n\
	\En este juego debes tratar de adivinar una secuencia de " ++ (show n) ++ " dígitos generadas por el programa.\n\
	\Para esto ingresas " ++ (show n) ++ " dígitos distintos con el fin de adivinar la secuencia.\n\
	\Si has adivinado correctamente la posición de un dígito se produce una Fama.\n\
	\Si has adivinado uno de los dígitos de la secuencia, pero en una posición distinta se trata de un Toque.\n\n\
	\Ejemplo: Si la secuencia es secuencia: [8, 0, 6, 1, 3] e ingresas 40863, entonces en pantalla aparecerá:\n\
	\tu ingresaste [4, 0, 8, 6, 3]\n\
  \resultado: 2 Toques 2 Famas\n\n"

toques_y_famas [] _ _ = (0,0)
toques_y_famas (n:ns) (x:xs) ys = if n == x then (t,1+f) else (if n `elem` ys then (t+1, f) else (t,f))
  where (t,f) = (toques_y_famas ns xs ys)

remover_dups xs = if (length xs) == (length num) then num else []
  where num = filter (<10) (nub (map digitToInt xs))

validar n xs = if (length num) == n then num else []
   where num = if not (all isDigit xs) then [] else remover_dups xs

jugar n xs i a ns t f 
      | f > 0 && f == n = do putStrLn $ "Ganaste! Acertaste al intento " ++ (show i) ++ "! La secuencia era " ++ (show xs)
      | a == "salir" = do putStrLn "\ngracias por jugar, adios."
      | otherwise = do
            when (i >0) $ if (i > 0 && null ns) 
                then do putStrLn $ "error!\n"
                else do 
                        putStrLn $ "tu ingresaste " ++ (show ns)
                        putStrLn $ "resultado: " ++ (show t) ++ " Toques, " ++ (show f) ++ " Famas\n"
            putStrLn $ "Ingresa una secuencia de " ++ (show n) ++ " dígitos distintos (o escribe salir):"
            acc <- getLine
            let num = (validar n acc)
              in if null num then jugar n xs (i+1) acc num 0 0 
                 else let (t,f) = (toques_y_famas num xs xs) 
                       in jugar n xs (i+1) acc num t f

main = let tam = 5 in do 
     sec <-  shuffleM [0..9]
     (mostrar_reglas tam)
     jugar tam (take tam sec) 0 "" [] 0 0