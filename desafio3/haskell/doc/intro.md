# ordenar_vector en Haskell

## Archivos

Leer el archivo de entrada y escribir el de salida aprovecha las propiedades lazy del Haskell:

Leer lineas:

        slineas <- sequence <$> map filtrar_linea <$> zip [0..] <$> LB.lines <$> LB.readFile entrada

Grabar resultado:

       	LB.writeFile salida $ LB.unlines  lineas


La magia ocurre al medio:

        lineas <- map ordenar_vector <$> slineas



## Slices y Arreglos de Bytes

Para trabajar con arreglos de bytes importamos:

	import qualified Data.ByteString.Lazy.Char8 as LB  

La parte más misteriosa es la función chunksOf

	chunksOf :: Int64 -> LB.ByteString -> [LB.ByteString]
	chunksOf x xs = unfoldr (justWhen not_null (L.splitAt x)) xs 


Esta función toma un arreglo de bytes lazy (LB.ByteString) y retorna un arreglos de Strings Lazy

Para esto usa la función L.splitAt.

Este es todo el código necesario para enteder chunksOf:


	justWhen :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
	justWhen f g a = if f a then Just (g a) else Nothing

	not_null = not . L.null

	chunksOf :: Int64 -> LB.ByteString -> [LB.ByteString]
	chunksOf x xs = unfoldr (justWhen not_null (L.splitAt x)) xs 

Para entender esto hay que recordar que L.splitAt n, retorna una tupla (x, xs) donde x son los primeros n elementos y xs el resto.


La función unfoldr f x va generando una lista mientras f x sea dstinto a Nothing. 

Lo que hace chunksOf es generar elementos en la lista aplicando L.splitAt a la lista.



## El ordenamiento

El código del ordenamiento es el siguiente:


	ordenar_vector :: LB.ByteString -> LB.ByteString
	ordenar_vector linea = 
	    let (encabezado, resto) = L.splitAt pos_vector linea
	        periodos = resultado $ ordenar_periodos $ chunksOf tam_periodo' resto
	        final    = LB.concat [encabezado,  LB.concat periodos]
	        pad      = LB.replicate (tam_vector - (LB.length final)) ' ' 
	    in LB.concat [final, pad]

Primero divide la linea entre encabezado y el resto. Luego crea los periodos y los ordena:

	ordenar_periodos $ chunksOf tam_periodo' resto

El ordenamiento de los periodos es el siguiente:

	ordenar_periodos :: [LB.ByteString] -> [LB.ByteString] 
	ordenar_periodos xs =  sortDesc $ nub $ (filter periodo_valido xs)


Esto hace un sort en orden descendiente de los periodos, luego de que ha eliminado duplicados y filtrado los periodos válidos.
Un periodo es válido si no es un periodo sólo con números cero:

	periodo_valido xs = LB.any (/= '0') xs

El vector ordenado se le antepone una letra en la función resultado:

	resultado xs 
	    | null xs = ["N"]
	    | (length xs) > elementos' = ["S"]
	    | otherwise = "D" :  take elementos' xs

Si el vector de periodos es vacío, se coloca una N, si tiene más de la cantidad de periodos máximos se coloca una S, de lo contrario se antepone una D a los datos.


Problema propuesto: ¿se puede hacer más rápido?
