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
toques_y_famas (n:ns) (x:xs) ys 
  | n == x = (t,1+f) 
  | n `elem` ys = (t+1, f)
  | otherwise = (t,f)
  where (t,f) = (toques_y_famas ns xs ys)

remover_dups xs = let num = filter (<10) $ nub $ map digitToInt xs
                  in if length xs == length num then num else []

validar n xs   
  | length num /= n = []
  | otherwise = num
   where num = if not (all isDigit xs) then [] else remover_dups xs

mostrar_resultado [] _ _ =  do putStrLn $ "error!\n"
mostrar_resultado ns t f =  do putStrLn $ "tu ingresaste " ++ (show ns)
                               putStrLn $ "resultado: " ++ (show t) ++ " Toques, " ++ (show f) ++ " Famas\n"

jugar n xs i a ns t f 
      | f == n = do putStrLn $ "Ganaste! Acertaste al intento " ++ (show i) ++ "! La secuencia era " ++ (show xs)
      | a == "salir" = do putStrLn "\ngracias por jugar, adios."
      | otherwise = do
            when (i > 0) $ mostrar_resultado ns t f             
            putStrLn $ "Ingresa una secuencia de " ++ (show n) ++ " dígitos distintos (o escribe salir):"
            acc <- getLine
            let num = validar n acc in
                if null num then jugar n xs (i+1) acc num 0 0 
                else let (t,f) = (toques_y_famas num xs xs) in jugar n xs (i+1) acc num t f

main = let tam = 5 in do 
     sec <-  shuffleM [0..9]
     putStrLn $ show $ take tam sec
     (mostrar_reglas tam)
     jugar tam (take tam sec) 0 "" [] 0 0