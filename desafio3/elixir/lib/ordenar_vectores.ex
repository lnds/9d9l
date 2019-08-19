defmodule Vectores do

	@largo_linea 838
	@pos_vector 9
	@largo_encabezado 9
	@largo_periodo 6
	@largo_vector 828
	@tam_vector 23
	@relleno "      "
	@relleno_largo String.duplicate(@relleno, @tam_vector)

	def main(argv) do
    	{time, _} = :timer.tc(fn () -> 
    		argv |> parse_args |> process 
    	end)

    	"tiempo ocupado: #{time/1_000_000.0} segundos" |> IO.puts
	end

	def parse_args(args) do
		if length(args) !== 2 do 
			:bad_args
		else 
			{:good_args, args}
		end
	end

	def process(:bad_args) do
		"uso: ordenar_vector archivo_entrada archivo_salida" |> IO.puts
	end

	def process({:good_args, [entrada,salida]}) do
		if not File.exists?(entrada) do
			"Archivo: #{entrada} no existe!" |> IO.puts
		else
			File.stream!(entrada)
				|> Stream.with_index
				|> Stream.map(&filtrar_linea/1)
				|> Stream.into(File.stream!(salida))
				|> Stream.run
		end
	end

	defp filtrar_linea({linea, n}) do
		if byte_size(linea) != @largo_linea do
			"Error largo incorrecto en linea #{n}" |> IO.puts
			linea
		else 
			encabezado = binary_part(linea, 0, @largo_encabezado)
			resto = binary_part(linea, @pos_vector, @largo_vector)
			vector = ordenar_vector(resto, 0, @largo_vector, MapSet.new)
			largo = MapSet.size(vector)
			cond do
				largo == 0 -> [encabezado, "N", @relleno_largo, "\n"]
				largo > @tam_vector -> [encabezado, "S",  @relleno_largo, "\n"]
				true -> [encabezado, "D", 
						vector |> MapSet.to_list |> Enum.sort(&(&2<&1)), 
						String.duplicate(@relleno, @tam_vector-largo), "\n"]
			end
		end
	end

	defp ordenar_vector(linea, pos, largo, periodos) do
		periodo = binary_part(linea, pos, @largo_periodo)
		if largo == @largo_periodo do
			agregar_periodo(periodo, periodos)
		else
			ordenar_vector(linea, pos+@largo_periodo, largo-@largo_periodo, agregar_periodo(periodo,periodos))
		end
	end

	defp agregar_periodo(periodo, periodos) do
		if periodo === <<"000000">> do
			periodos
		else
			MapSet.put(periodos, periodo)
		end
	end
end
