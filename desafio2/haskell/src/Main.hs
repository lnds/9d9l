module Main (main) where 

import System.Environment

process_par api_key args = putStrLn $ "par ak="++(show api_key)++" args = "++(show args)

api_call :: String -> String -> String
api_call api_key city = 
     "calling" ++ "http://api.openweathermap.org/data/2.5/weather?q="++city++"&mode=xml&units=metric&lang=sp&appid="++api_key

print_reports :: [String] -> IO()
print_reports [] = putStrLn ""
print_reports (r:xs) = 
    do putStrLn $ (show r)
       print_reports xs

process_seq api_key args = 
    print_reports $ (map (api_call api_key) args)

process_args [] _ = putStrLn "debe configurar la variable de ambiente WEATHER_API_KEY"
process_args _ [] = putStrLn "debe ingresar una lista de ciudades"
process_args api_key (p:args)
    | p == "-p" && (null args) =putStrLn "debe ingresar una lista de ciudades"
    | p == "-p" = process_par api_key args
    | otherwise = process_seq api_key (p:args)


main = do
    api_key <- getEnv "WEATHER_API_KEY"
    args <- getArgs 
    process_args api_key args

