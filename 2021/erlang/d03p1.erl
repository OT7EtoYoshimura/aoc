-module(d3p1).
-export([p1/0]).
-include("../include/aoc2021.hrl").

p1() ->
	{ok, File} = file:read_file(?IN),
	BinaryList = binary:split(File, <<"\n">>, [global, trim_all]),
	CountOnes =
	lists:foldl(
	  fun(Elem, [FirstC, SecondC, ThirdC, FourthC, FifthC, SixthC, SeventhC,
							 EighthC, NinthC, TenthC, EleventhC, TwelvethC]) ->
			  <<First:8, Second:8, Third:8, Fourth:8, Fifth:8, Sixth:8, Seventh:8,
					Eighth:8, Ninth:8, Tenth:8, Eleventh:8, Twelveth:8>> = Elem,
			  [
			   if <<First>> == <<"1">>		->  FirstC+1;		true	-> FirstC end,
			   if <<Second>> == <<"1">>		->  SecondC+1;		true	-> SecondC end,
			   if <<Third>> == <<"1">>		->  ThirdC+1;		true	-> ThirdC end,
			   if <<Fourth>> == <<"1">>		->  FourthC+1;		true	-> FourthC end,
			   if <<Fifth>> == <<"1">>		->  FifthC+1;		true	-> FifthC end,
			   if <<Sixth>> == <<"1">>		->  SixthC+1;		true	-> SixthC end,
			   if <<Seventh>> == <<"1">>	->  SeventhC+1;		true	-> SeventhC end,
			   if <<Eighth>> == <<"1">>		->  EighthC+1;		true	-> EighthC end,
			   if <<Ninth>> == <<"1">>		->  NinthC+1;		true	-> NinthC end,
			   if <<Tenth>> == <<"1">>		->  TenthC+1;		true	-> TenthC end,
			   if <<Eleventh>> == <<"1">>	->  EleventhC+1;	true	-> EleventhC end,
			   if <<Twelveth>> == <<"1">>	->  TwelvethC+1;	true	-> TwelvethC end
			  ]
	  end,
	  [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
	  BinaryList),
	Mid = length(BinaryList) / 2,
	GammaEpsilon = lists:map(
	  fun(Elem) when Elem > Mid -> {<<"1">>, <<"0">>};
		 (_)	-> {<<"0">>, <<"1">>} end,
	  CountOnes),
	{GammaBits, EpsilonBits} = lists:unzip(GammaEpsilon),
	binary_to_integer(list_to_binary(GammaBits), 2)
	* binary_to_integer(list_to_binary(EpsilonBits), 2).

