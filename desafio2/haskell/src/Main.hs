module Main (main) where 

import Network.HTTP
import System.Environment
import Data.Maybe
import Data.List
import Text.Printf
import Text.XML.Light

data WeatherReport = Report String Float Float Float String 

api_call api_key city = 
    simpleHTTP (getRequest url) >>= getResponseBody
    where
        url = "http://api.openweathermap.org/data/2.5/weather?q="++city++"&mode=xml&units=metric&lang=sp&appid="++api_key

print_reports [] = putStrLn ""
print_reports (r:rs) = 
  do print r
     print_reports rs
  where  print (Report c t max min w) = printf "%-30s max:%5.1f  min:%5.2f   actual: %5.1f %s\n" c max min t w

process_par api_key args = putStrLn $ "par ak="++(show api_key)++" args = "++(show args)

xmlRead elem attr = 
  head . concatMap (map (fromJust.findAttr (unqual attr)) . filterElementsName (== unqual elem)) . onlyElems . parseXML
          
make_report x = 
  do val <- x
     return (Report (name val) (temp val) (max val) (min val) (weather val))
  where 
      name val = (xmlRead "city" "name" $ val)
      temp val = read (xmlRead "temperature" "value" $ val) :: Float
      max val  = read (xmlRead "temperature" "max" $ val) :: Float
      min val  = read (xmlRead "temperature" "min" $ val) :: Float
      weather val = (xmlRead "weather" "value" $ val)

cmp_rep :: WeatherReport -> WeatherReport -> Ordering
cmp_rep (Report _ t1 _ _ _) (Report _ t2 _ _ _)  = compare t2 t1


process_seq api_key args =
    do lreps <- reps
       print_reports $  sortBy cmp_rep lreps 
    where reps = mapM (make_report . (api_call api_key)) args -- mapM => [IO r] -> IO [r]

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
