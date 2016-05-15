module Main (main) where 

import Network.HTTP
import System.Environment
import Data.Maybe
import Text.XML.Light

data WeatherReport = Report String Float Float Float String 
    deriving Show

api_call api_key city = 
    simpleHTTP (getRequest url) >>= getResponseBody
    where
        url = "http://api.openweathermap.org/data/2.5/weather?q="++city++"&mode=xml&units=metric&lang=sp&appid="++api_key

show_report :: WeatherReport -> String
show_report (Report city temp max min weather) =
  city ++ "max: " ++ (show max) ++ "min: " ++ (show min) ++ " actual: " ++ (show temp) ++ " " ++ weather

print_reports :: [IO WeatherReport] -> IO ()
print_reports [] = putStrLn ""
print_reports (r:xs) = 
  do rep <- r
     putStrLn $ (show_report rep)
     print_reports xs
     return ()

process_par api_key args = putStrLn $ "par ak="++(show api_key)++" args = "++(show args)

xmlRead elem attr = 
  head . concatMap (map (fromJust.findAttr (unqual attr)) . filterElementsName (== unqual elem)) . onlyElems . parseXML
          
make_report :: IO String -> IO WeatherReport
make_report x = 
  do val <- x
     return (Report (name val) (temp val) (max val) (min val) (weather val))
  where 
        name val = (xmlRead "city" "name" $ val)
        temp val = read (xmlRead "temperature" "value" $ val) :: Float
        max val  = read (xmlRead "temperature" "max" $ val) :: Float
        min val  = read (xmlRead "temperature" "min" $ val) :: Float
        weather val = (xmlRead "weather" "value" $ val)


process_seq api_key args = 
  print_reports (map make_report (map (api_call api_key) args))

process_args [] _ = putStrLn "debe configurar la variable de ambiente WEATHER_API_KEY"
process_args _ [] = putStrLn "debe ingresar una lista de ciudades"
process_args api_key (p:args)
    | p == "-p" && (null args) = putStrLn "debe ingresar una lista de ciudades"
    | p == "-p" = process_par api_key args
    | otherwise = process_seq api_key (p:args)


main = do
    api_key <- getEnv "WEATHER_API_KEY"
    args <- getArgs 
    process_args api_key args

