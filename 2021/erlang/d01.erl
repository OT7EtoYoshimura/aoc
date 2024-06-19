-module(d1).
-export([p1/0, p2/0, both/0,
		 p1_spawn/0, p2_spawn/0, both_spawn/0, both_parallel_spawn/0]).
-include("../include/aoc2021.hrl").
-define(FILENAME, ?IN).

-spec p1_tco(List :: [integer()], Count :: integer()) -> integer().
-spec p2_tco(List :: [integer()], Count :: integer()) -> integer().
-spec both_tco(List :: [integer()], CountP1 :: integer(), CountP2 :: integer())
	-> integer().

p1()		-> p1_tco(fromFile(?FILENAME), 0).
p2()		->	p2_tco(fromFile(?FILENAME), 0).
both()	-> both_tco(fromFile(?FILENAME), 0, 0).

p1_spawn()		-> spawn(fun() -> p1_tco(fromFile(?FILENAME), 0) end).
p2_spawn()		-> spawn(fun() -> p2_tco(fromFile(?FILENAME), 0) end).
both_spawn()	-> spawn(fun() -> both_tco(fromFile(?FILENAME), 0, 0) end).
both_parallel_spawn() ->
	List = fromFile(?FILENAME),
	spawn(fun() -> p1_tco(List, 0) end),
	spawn(fun() -> p2_tco(List, 0) end).

both_tco([_T], CountP1, CountP2) ->
	io:format("P1: ~p~nP2: ~p~n", [CountP1, CountP2]);
both_tco([_H1|T] = [H1|[H2|[_H3|[H4|_T2]]]], CountP1, CountP2) ->
	both_tco(T,
					 if H1 < H2 -> CountP1+1; true -> CountP1 end,
					 if H1 < H4 -> CountP2+1; true -> CountP2 end);
both_tco([_H1|T] = [H1|[H2|_T2]], CountP1, CountP2) when H1 < H2 ->
	both_tco(T, CountP1+1, CountP2);
both_tco([_H1|T], CountP1, CountP2) -> both_tco(T, CountP1, CountP2).

p1_tco([_T], Count) ->
	io:format("P1: ~p~n", [Count]);
p1_tco([_H1|T] = [H1|[H2|_T2]], Count) when H1 < H2	->
	p1_tco(T, Count+1);
p1_tco([_H1|T], Count)->
	p1_tco(T, Count).

p2_tco(List, Count) when length(List) < 4 ->
	io:format("P2: ~p~n", [Count]);
p2_tco([_H1|T] = [H1|[_H2|[_H3|[H4|_T2]]]], Count) when H1 < H4	->
	p2_tco(T, Count+1);
p2_tco([_H1|T], Count) ->
	p2_tco(T, Count).

fromFile(FileName) ->
	{ok, File} = file:read_file(FileName),
	BinaryList = binary:split(File, <<"\n">>, [global, trim_all]),
	lists:map(fun(Elem) -> binary_to_integer(Elem) end, BinaryList).
