{-#  LANGUAGE OverloadedStrings #-}
module Main(main) where 

import Data.Int (Int64)
import Text.Printf
import Formatting
import Formatting.Clock
import System.Clock
import System.Environment
import System.IO
import Data.Maybe
import Data.List
import Data.List.Split
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as LB  
import qualified Data.ByteString.Lazy as L



justWhen :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
justWhen f g a = if f a then Just (g a) else Nothing

nothingWhen :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
nothingWhen f = justWhen (not . f)

chunksOf' :: Int64 -> LB.ByteString -> [LB.ByteString]
chunksOf' x = unfoldr (nothingWhen L.null (L.splitAt x)) 

all_0 :: LB.ByteString -> Bool
all_0 xs = LB.all (== '0') xs

resultado xs 
    | (length xs) == 0 = ["N"]
    | (length xs) > tam_vector = ["S"]
    | otherwise = "D" : (reverse $ take tam_vector xs)
    where tam_vector = 23

ordenar :: [LB.ByteString] -> [LB.ByteString]
ordenar xs = resultado $ sort $ nub $ (filter (not . all_0) xs)

ordenar_vector linea = 
    let encabezado = LB.take pos_vector linea
        resto = LB.drop pos_vector linea
        periodos = ordenar $ chunksOf' tam_periodo resto
        final = LB.concat [encabezado,  LB.concat periodos]
        pad = LB.replicate (tam_vector - (LB.length final)) ' ' 
    in LB.concat [final, pad]
    where pos_vector = 9
          tam_periodo = 6
          tam_vector = 23 * 6 + 9 + 1

--filtrar_lineas :: (Int, LB.ByteString) -> IO LB.ByteString
--filtrar_lineas ::  LB.ByteString ->   LB.ByteString
filtrar_lineas  linea = do
    when ((LB.length linea) /= tam_linea) $ putStrLn  "error: " 
    return linea
    where tam_linea = 9 + 23 * 6 * 6

procesar_vectores :: [String] -> IO()
procesar_vectores (entrada:salida:[]) = do
    slineas <- sequence <$> map filtrar_lineas <$> LB.lines <$> LB.readFile entrada
    lineas <- map ordenar_vector <$> slineas
    let content = LB.unlines lineas
    LB.writeFile salida content

procesar_vector _ = do putStrLn  "uso: ordenar_vector archivo_entrada archivo_salida"

main = do
    t1 <- getTime Monotonic
    args <- getArgs
    procesar_vectores args
    t2 <- getTime Monotonic
    printf "tiempo ocupado: %s\n" (format timeSpecs t1 t2)