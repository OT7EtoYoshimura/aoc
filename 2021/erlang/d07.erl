-module(d7).
-export([main/0]).
-include("../include/aoc2021.hrl").

main() ->
	{_, Raw}   = file:read_file(?IN),
	CrabsPos   = lists:map(fun binary_to_integer/1, string:lexemes(Raw, ",\n")),
	CrabsCount = length(CrabsPos),
	Median     = lists:nth(round(CrabsCount/2), lists:sort(CrabsPos)),
	Mean       = lists:sum(CrabsPos) / CrabsCount,
	Fuel1      =     lists:foldl(fun(Elem, Sum) ->
																	 Sum +          abs(Median     -Elem)
															 end, 0, CrabsPos),
	Fuel2      = min(lists:foldl(fun(Elem, Sum) ->
																	 Sum + ints_sum(abs( ceil(Mean)-Elem))
															 end, 0, CrabsPos),
	                 lists:foldl(fun(Elem, Sum) ->
																	 Sum + ints_sum(abs(floor(Mean)-Elem))
															 end, 0, CrabsPos)),
	{{p1, Fuel1}, {p2, trunc(Fuel2)}}.
