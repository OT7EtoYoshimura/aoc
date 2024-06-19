-module(d4).
-export([main/0]).
-include("../include/aoc2021.hrl").

-type chosen()	:: non_neg_integer().
-type score()		:: non_neg_integer().
-type status()	:: marked | unmarked.
-type cell()		:: {Guess :: non_neg_integer(), status()}.
-type row()			:: [cell()].
-type board()		:: [row()].

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Impure IO Garbage %%%
%%%%%%%%%%%%%%%%%%%%%%%%%

main() ->
	{_, Data} = file:read_file(?IN),
	[ChosenNumsRaw|BoardsRaw] = binary:split(Data, <<"\n\n">>,
																					 [global, trim_all]),
	ChosenNums = lists:map(fun binary_to_integer/1,
												 string:lexemes(ChosenNumsRaw, ",")),
	BoardsRawList = [binary:split(BoardRaw, <<"\n">>, [global, trim_all])
									 || BoardRaw <- BoardsRaw],
	%% God, forgive me for this next line.
%	Boards = lists:map(fun(Board) ->
%												 lists:map(fun(Row) ->
%																			 lists:map(fun(GuessRaw) ->
%																										 {binary_to_integer(GuessRaw), unmarked}
%																								 end, string:lexemes(Row, " "))
%																	 end, Board)
%										 end, BoardsRawList),
	%% Pick your poison
	Boards = [[[ {binary_to_integer(GuessRaw), unmarked}
							 || GuessRaw <- string:lexemes(Row, " ")]
						 || Row <- Board]
						|| Board <- BoardsRawList],
	markBoards(Boards, ChosenNums).

%%%%%%%%%%%%%%%%%%%%%%
%%% The Good Stuff %%%
%%%%%%%%%%%%%%%%%%%%%%

-spec markBoards([board() | score()], [chosen()])
																-> {{p1, score()}, {p2, score()}};
								([score()], []) -> {{p1, score()}, {p2, score()}}.
-spec markBoard(score(), any()) -> score();
							 (board(), chosen())	-> score() | board().
-spec markRow(row(),     chosen())	-> row().
-spec markCell(cell(),   chosen())	-> cell().
-spec checkBoard(board())   -> true | false.
-spec checkRow(row())       -> true | false.
-spec checkCell(cell())     -> true | false.
-spec sum([] | [cell()], Acc :: non_neg_integer()) -> non_neg_integer().

markBoards(Boards, [ChosenNum|ChosenNums]) ->
	MarkedBoards = lists:map(fun(Board) ->
															 markBoard(Board, ChosenNum)
													 end, Boards),
	markBoards(MarkedBoards, ChosenNums);
markBoards(Scores,[]) ->
	{{p1, lists:last(Scores)}, {p2, hd(Scores)}}.
markBoard(Score, _) when is_integer(Score) -> Score;
markBoard(Board, ChosenNum) ->
	MarkedBoard = lists:map(fun(Row) -> markRow(Row, ChosenNum) end, Board),
	case checkBoard(MarkedBoard) of
		true  ->
			ChosenNum * sum(lists:flatten(MarkedBoard), 0);
		false -> MarkedBoard
	end.
markRow(Row, ChosenNum) ->
	lists:map(fun(Cell) -> markCell(Cell, ChosenNum) end, Row).
markCell({Guess, _}, ChosenNum) when Guess =:= ChosenNum ->
	{Guess, marked};
markCell(Cell, _) -> Cell.

checkBoard(Board) ->
	lists:any(fun checkRow/1, Board) orelse
	lists:any(fun checkRow/1, transpose(Board)).
checkRow(Row) ->
	lists:all(fun checkCell/1, Row).
checkCell({_, marked})	-> true;
checkCell(_) 			-> false.

sum([], Acc) ->
	Acc;
sum([{Guess, unmarked}|T], Acc) ->
	sum(T, Acc+Guess);
sum([_|T], Acc) ->
	sum(T, Acc).

-spec transpose([[]|_])					-> [];
               (M :: [list()])  -> [list()].
transpose([[]|_]) -> [];
transpose(M) ->
  [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].
