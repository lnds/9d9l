{-#  LANGUAGE OverloadedStrings #-}
module Main(main) where 

import Data.Int (Int64)
import Text.Printf
import Formatting
import Formatting.Clock
import System.Clock
import System.Environment
import Data.List
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LB  
import qualified Data.ByteString.Lazy as L

pos_vector = 9
tam_periodo = 6
instituciones = 6
elementos = 23 
elementos' = 23 :: Int

tam_vector = elementos * tam_periodo + pos_vector + 1
tam_linea = pos_vector + elementos * tam_periodo * instituciones

justWhen :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
justWhen f g a = if f a then Just (g a) else Nothing

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

not_null :: LB.ByteString -> Bool
not_null = not . L.null

chunksOf :: Int64 -> LB.ByteString -> [LB.ByteString]
chunksOf x xs = unfoldr (justWhen not_null (L.splitAt x)) xs 

periodo_valido :: LB.ByteString -> Bool
periodo_valido xs = LB.any (/= '0') xs

clasificar_resultado :: [LB.ByteString] -> [LB.ByteString]
clasificar_resultado xs 
    | null xs = ["N"]
    | (length xs) > elementos' = ["S"]
    | otherwise = "D" :  take elementos' xs

ordenar_periodos :: [LB.ByteString] -> [LB.ByteString] 
ordenar_periodos xs =  sortDesc $ nub $ (filter periodo_valido xs)

ordenar_vector :: LB.ByteString -> LB.ByteString
ordenar_vector linea  
    | (LB.length linea) /= tam_linea = linea
    | otherwise = LB.concat [final, pad] 
        where (encabezado, resto) = L.splitAt pos_vector linea
              periodos = clasificar_resultado $ ordenar_periodos $ chunksOf tam_periodo resto
              final    = LB.concat [encabezado,  LB.concat periodos]
              pad      = LB.replicate (tam_vector - (LB.length final)) ' ' 

filtrar_linea :: (Int, LB.ByteString) -> IO LB.ByteString
filtrar_linea (n,linea) = do
            when ((LB.length linea) /= tam_linea) $ putStrLn $ "!!! Largo incorrecto en linea:: " ++ (show n)
            return linea

procesar_vectores :: String -> String -> IO()
procesar_vectores entrada salida = do
        slineas <- sequence <$> map filtrar_linea <$> zip [0..] <$> LB.lines <$> LB.readFile entrada
        lineas <- map ordenar_vector <$> slineas
        LB.writeFile salida $ LB.unlines  $ lineas
        
main = do
    t1 <- getTime Monotonic
    args <- getArgs
    if length args /= 2 then do putStrLn  "uso: ordenar_vector archivo_entrada archivo_salida"
    else procesar_vectores (head args) (last args)
    t2 <- getTime Monotonic
    printf "tiempo ocupado: %s\n" (format timeSpecs t1 t2)