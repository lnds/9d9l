-module (huffman).
-compile({no_auto_import,[size/1]}).
-export ([main/0, main/1]).
-import(filename, [absname/1]).

-define(USAGE, "uso: huffman [c|d] archivo_entrada archivo_salida\n").

main() -> io:format(?USAGE).
main([Opt,Entrada,Salida]) when Opt =:= 'c' -> comprimir(Entrada, Salida);
main([Opt,Entrada,Salida]) when Opt =:= 'd' -> descomprimir(Entrada, Salida);
main([_]) -> io:format(?USAGE).



comprimir(Entrada, Salida) -> 
	{ok, Binary} = file:read_file(Entrada),
	Bytes = binary_to_list(Binary),
	{Dump, Tree} = encode(Bytes),
	DTree = tree_as_bits(Tree),
	PS = (8 - ((bit_size(DTree) + bit_size(Dump)) rem 8)) rem 8,
	PDump = <<DTree:(bit_size(DTree))/bitstring, Dump:(bit_size(Dump))/bitstring, 0:PS>>,
	ok = file:write_file(Salida, PDump).

encode(Bytes) ->
	Freqs = calc_freqs(Bytes),
	Tree = build_tree(Freqs),
	Dict = dict:from_list(build_codes(Tree)),
	Dump = << <<(dict:fetch(Char, Dict))/bits>> || Char <- Bytes >>,
	{Dump, Tree}.

% build a huffman tree
build_tree([{Node, _} | []]) -> Node; %only one
build_tree(Nodes) ->
	[{L1, F1}, {L2, F2} | Rest] = lists:keysort(2, Nodes),
	build_tree([{{L1, L2}, F1+F2} | Rest]).

tree_as_bits({L,R}) -> 
	BL = tree_as_bits(L),
	BR = tree_as_bits(R),
	<<0:1, BL/bitstring, BR/bitstring>>;
tree_as_bits(Symbol) -> 
	<<1:1, Symbol>>.

% build a tree from file

read_tree(<<1:1, Rest/bitstring>>) -> read_leaf(Rest);
read_tree(<<0:1, Rest/bitstring>>) -> read_node(Rest).

read_leaf(<<Sym:8, Rest/bitstring>>) -> {{Sym,-1}, Rest}.

read_node(Bits) -> 
	{L, Rest1} = read_tree(Bits),
	{R, Rest2} = read_tree(Rest1),
	{{L,R}, Rest2}.

%build_codes from a tree build all codes
build_codes({L,R}) ->
	build_codes(L, <<0:1>>) ++ build_codes(R, <<1:1>>).

build_codes({L,R}, <<Bits/bits>>) ->
	build_codes(L, <<Bits/bits, 0:1>>) ++ build_codes(R, <<Bits/bits, 1:1>>);
build_codes(Symbol, <<Bits/bits>>) ->
	[{Symbol, Bits}].

% calculate frequencies on a list of bytes
calc_freqs(Data) ->
	calc_freqs(lists:sort(Data), []). % sort them in order to group same bytes

calc_freqs([], Acum) ->
	Acum;
calc_freqs([Head|Tail], Acum) ->
	{Block, Rest} = lists:splitwith(fun (X) -> X == Head end, Tail),
	calc_freqs(Rest, [{Head, 1+length(Block)} | Acum]).

decode(Code, Tree) -> 
	L = [X || <<X:1>> <= Code],
	decode(L, Tree, Tree, []).

decode([], _, _, Result) -> 
	lists:reverse(Result);

decode([1|Rest], {_, R={_,_}}, Tree, Result) ->
	decode(Rest, R, Tree, Result);

decode([0|Rest], {L={_,_}, _}, Tree, Result) ->
	decode(Rest, L, Tree, Result);

decode(L, {Sym,-1},  Tree, Result) -> 
	decode(L, Tree, Tree, [Sym|Result]).

descomprimir(Entrada, Salida) -> 
	{ok, Binary} = file:read_file(Entrada),
	{Tree, Code} = read_tree(Binary),
	Dump = decode(Code, Tree),
	ok = file:write_file(Salida, Dump).
