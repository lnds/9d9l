-module (weather).
-export ([main/1, buscar_reportes_con_map_async/1]).

-define(UA, "Mozilla/5.0 (Erlang http:request)").
-define(API_KEY, os:getenv("WEATHER_API_KEY")).

main([]) -> io:format("debe ingresar una lista de ciudades\n");
main([P]) when P =:= 'p' -> io:format("debe ingresar una lista de ciudades\n");
main([P|Args]) when P =:= 'p'  -> 
	application:start(inets),
	cronometrar(fun () ->
		REPS = lists:sort(fun (A,B) -> comparar(A,B) end, buscar_reportes_con_map_async(Args)), 
		imprimir_reportes(REPS)
	end);

main(Args) -> 
	application:start(inets),
	cronometrar(fun () ->
		REPS = lists:sort(fun (A,B) -> comparar(A,B) end, buscar_reportes_con_map(Args)), 
		imprimir_reportes(REPS)
	end).

comparar(A,B) ->
	{K1,_,T1,_,_,_} = A,
	{K2,_,T2,_,_,_} = B,
	if (K1 =:= ok) and (K2 =:= ok) -> T1 > T2;
	   true -> if K1 =:= ok -> true; K1 =:= error -> false end
	end.

imprimir_reportes([]) -> ok;
imprimir_reportes([H|T]) -> imprimir_reporte(H), imprimir_reportes(T).

imprimir_reporte(R) ->
	{T,Ciudad,Temp,Max,Min,Cond} = R, 
	if T =:= ok -> io:format("~-30.30s max:~5.1f  min: ~5.1f   actual: ~5.1f ~s\n", [Ciudad, Max, Min, Temp, Cond]);
	   T =:= error -> io:format("~s error: ~s\n", [Ciudad,Cond])
	end.

extraer_valor([], _) -> error;
extraer_valor([H|T], Attr) ->
	{xmlAttribute,A,_,_,_,_,_,_,Value,_} = H,
	if A =:= Attr -> Value;
	   A =/= Attr -> extraer_valor(T, Attr)
	end. 

extraer_reporte(Xml) ->
	{Root,_} = xmerl_scan:string(Xml),
	Ciudad = extraer_valor(xmerl_xpath:string("//city/@*", Root), name),
	TempAttrs = xmerl_xpath:string("//temperature/@*", Root),
	%% truco para el caso en que la temperatura es entera
	{Temp,_} = string:to_float(extraer_valor(TempAttrs, value) ++ ".0"),
	{Max,_} = string:to_float(extraer_valor(TempAttrs, value) ++ ".0"),
	{Min,_} = string:to_float(extraer_valor(TempAttrs, value) ++ ".0"),
	Cond = extraer_valor(xmerl_xpath:string("//weather/@*", Root), value),
	{ok,Ciudad,Temp,Max,Min,Cond}.

buscar_reportes_con_map(Ciudades) -> 
	lists:map(
		fun(Ciudad) ->
			Url = crear_url_api(Ciudad),
			Xml = llamar_api(Url),
			extraer_reporte(Xml)	
		end,
		Ciudades).

buscar_reportes_con_map_async(Ciudades) -> 
	ReqIds = lists:map(
		fun(Ciudad) ->
			Url = crear_url_api(Ciudad),
			{Ciudad,llamar_api_async(Url)}
		end,
		Ciudades),
	recolectar(ReqIds).

recolectar([Llam|Llams]) -> 
	{Ciudad,ReqId} = Llam,
    receive
        {http, {ReqId, {{_, Code, _}, _, Body}}} -> 
        	[if Code < 400-> extraer_reporte(binary_to_list(Body)); 
        		Code > 399 -> {error,Ciudad,0.0,0.0,0.0,"error llamando api"}
        	 end 
        	| recolectar(Llams)]
    end;
recolectar([]) -> [].

crear_url_api(Ciudad) ->
	io_lib:format("http://api.openweathermap.org/data/2.5/weather?q=~s&mode=xml&units=metric&lang=sp&appid=~s",[Ciudad,?API_KEY]).

llamar_api(Url) ->
	timer:sleep(150),
	{ok, {{_,Code,_}, _, Body}} = httpc:request(Url),
	if Code < 400-> Body;
       Code > 399 -> []
    end.

llamar_api_async(Url) ->
	timer:sleep(150),
	{ok, ReqId} = httpc:request(get, {Url, [{"User-Agent", ?UA}]},[], [{sync, false}]),
	ReqId.

cronometrar(F) -> 
    statistics(wall_clock),
    F(),
    {_, RTime} = statistics(wall_clock),
    io:format("tiempo ocupado para generar el reporte: 0:0:~.3f ~n", [RTime / 1000.0]).
