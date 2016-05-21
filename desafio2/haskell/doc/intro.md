# Weather en Haskell

## Cliente HTTP

Hay varias formas de invocar http en Haskell.
Optamos por una de las más comunes: Network.HTTP

    api_call api_key city = 
        simpleHTTP (getRequest url) >>= getResponseBody
        where
            url = "http://api.openweathermap.org/data/2.5/weather?q="++city++"&mode=xml&units=metric&lang=sp&appid="++api_key

## Ejecución en paralelo

El truco fue usar Control.Monad.Parallel de este modo la función process_seq y process_par son identicas, salvo  al invocar a mapM 


    process_seq api_key args =
        do lreps <- reps
           print_reports $  sortBy cmp_rep lreps 
        where reps = mapM (make_report . (api_call api_key)) args -- mapM => [IO r] -> IO [r]


    process_par api_key args =
        do lreps <- preps
           print_reports $ sortBy cmp_rep lreps 
        where preps = P.mapM (make_report . (api_call api_key)) args -- mapM => [IO r] -> IO [r]

## Parsing XML

El parsing XML es lo más oscuro del código. Usamos Text.XML.Light.

    xmlRead elem attr = 
      head . concatMap (map (fromJust.findAttr (unqual attr)) . filterElementsName (== unqual elem)) . onlyElems . parseXML

Esta función devuelve una función que evalúa una lista con el contenido de los atributos (attr) dentro del elemento elem.