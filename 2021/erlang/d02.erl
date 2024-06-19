-module(d2).
-export([p1/0, p2/0]).
-include("../include/aoc2021.hrl").

p1() ->
	ListOfTuples = read(?IN),
	p1(ListOfTuples, 0, 0).
p1([], Horizontal, Depth) ->
	io:format("Horizontal: ~p~nDepth: ~p~nMultiplied: ~p~n",
						[Horizontal, Depth, Horizontal*Depth]);
p1([{forward, Amount}|T], Horizontal, Depth) ->
	p1(T, Horizontal+Amount, Depth);
p1([{down, Amount}|T], Horizontal, Depth) ->
	p1(T, Horizontal, Depth+Amount);
p1([{up, Amount}|T], Horizontal, Depth) ->
	p1(T, Horizontal, Depth-Amount).

p2() ->
    ListOfTuples = read(?IN),
    p2(ListOfTuples, 0, 0, 0).
p2([], Horizontal, Depth, _Aim) ->
    io:format("Horizontal: ~p~nDepth: ~p~nMultiplied: ~p~n",
							[Horizontal, Depth, Horizontal*Depth]);
p2([{forward, Amount}|T], Horizontal, Depth, Aim) ->
    p2(T, Horizontal+Amount, Depth+(Aim*Amount), Aim);
p2([{down, Amount}|T], Horizontal, Depth, Aim) ->
    p2(T, Horizontal, Depth, Aim+Amount);
p2([{up, Amount}|T], Horizontal, Depth, Aim) ->
    p2(T, Horizontal, Depth, Aim-Amount).


read(FileName) ->
	{ok, File} = file:read_file(FileName),
	BinaryList = binary:split(File, <<"\n">>, [global, trim_all]),
	ListOfLists = [binary:split(X, <<" ">>, []) || X <- BinaryList],
	[{binary_to_atom(Dir), binary_to_integer(Amount)}
	 || [Dir,Amount] <- ListOfLists].
