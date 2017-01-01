# Ordenar Vectores en Erlang

Claramente Erlang no es un lenguaje diseñado para este tipo de labores, y sus bibliotecas estándar no tienen mucho soporte para esto.

La primera ejecución de este programa tomaba casi 4 minutos.

He logrado reducir el tiempo a cerca de 45 segundos, lo que considero suficiente.


Para ejecutar en paralelo la llamada a la api usamos la primitiva spawn de Erlang, de la siguiente manera

    llamar_api_async(Url) -> spawn(?MODULE, async_api_call, [Url]).

    async_api_call(Url) ->
        % io:format("llamando asincronamenete url: ~s\n", [Url]), % descomentar para convencerse que es asincrono
        {Xml,Error} = llamar_api(Url),
        receive 
            {From, get_result} -> From ! {xml, Xml, Error}
        end.

La función llamar_api_async/1 ejecuta en forma asíncrona la función async_api_call/1 con parámetro Url.

La función async_api_call/1 llama a la API en forma sincrona y se queda esperando un mensaje de la forma:
    {Pid, get_result}

La función recolectar/1 tiene la siguiente pieza de código: 

    ReqId ! {self(), get_result},
    receive
        {xml, Xml, Error} -> 
            [extraer_reporte(Xml, Ciudad, Error) | recolectar(Llams)]
    end;

Esto hace que le enviemos el mensaje a async_api_call/1 para que nos envíe el resultado. La función async_api_call responde con otro mensaje:
    receive 
            {From, get_result} -> From ! {xml, Xml, Error}
    end.

El que recibimos con
    receive
        {xml, Xml, Error} -> 
            [extraer_reporte(Xml, Ciudad, Error) | recolectar(Llams)]
    end;

