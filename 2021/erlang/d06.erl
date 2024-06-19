-module(d6).
-export([main/0]).
-include("../include/aoc2021.hrl").
-define(DAYS, 256).

main() ->
	{_, Raw} = file:read_file(?IN),
	Fish = [list_to_integer(X)
		|| X <- string:lexemes(lists:droplast(binary_to_list(Raw)), ",")],
	simulate(fishMap(Fish), ?DAYS).

fishMap(FishList) ->
	fishMap(maps:from_keys(lists:seq(0,8), 0), FishList).
fishMap(FishMap, []) ->
	FishMap;
fishMap(FishMap, [H|T]) ->
	fishMap(maps:update_with(H, fun(V)->V+1 end, FishMap), T).

simulate(FishMap, 0) ->
	maps:fold(fun(_,V,Acc) -> V+Acc end, 0, FishMap);
simulate(FishMap, Day) ->
	simulate(tick(#{}, FishMap, 8), Day-1).

tick(NewMap, OldMap, 0) ->
	#{0 := Breedable} = OldMap,
	NewMap#{8 => Breedable};
tick(NewMap, OldMap, 7) ->
	#{7 := OldValue, 0 := Breedable} = OldMap,
	tick(NewMap#{6 => OldValue+Breedable}, OldMap, 6);
tick(NewMap, OldMap, Day) ->
	#{Day := OldValue} = OldMap,
	tick(NewMap#{Day-1 => OldValue}, OldMap, Day-1).
