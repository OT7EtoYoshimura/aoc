-module(d16).
-export([main/1, main/0]).
-include("../include/aoc2021.hrl").
-type packet()
    :: {literal,  Ver :: bitstring(), Val  :: uint()}
    |  {operator, Ver :: bitstring(), Type :: 0..7, [packet()]}.

-spec main() -> {{p1,uint()}, {p2, uint()}}.
main() -> main(?IN).
-spec main(bitstring() | string()) -> {{p1, uint()},{p2, uint()}}.
main(Bits) when is_bitstring(Bits) ->
    {Packet, _Padding} = parse_packet(binary:decode_hex(Bits)),
    {{p1, ver_sum(Packet,0)}, {p2, eval(Packet)}};
main(Filename) ->
    {_, Raw} = file:read_file(Filename),
    Trimmed  = string:trim(Raw),
    main(Trimmed).

-spec ver_sum(packet(), uint()) -> uint().
ver_sum({literal, Ver, _}, Acc) ->
    Acc + Ver;
ver_sum({operator, Ver, _, Packets}, Acc) ->
    lists:foldl(fun ver_sum/2, Acc + Ver, Packets).

-spec eval(packet()) -> uint().
eval({literal, _, Val}) -> Val;
eval({operator, _, 0, Operands}) ->
    Helper = fun(Operand, Acc) -> eval(Operand) + Acc end,
    lists:foldl(Helper, 0, Operands);
eval({operator, _, 1, Operands}) ->
    Helper = fun(Operand, Acc) -> eval(Operand) * Acc end,
    lists:foldl(Helper, 1, Operands);
eval({operator, _, 2, Operands}) -> lists:min(lists:map(fun eval/1, Operands));
eval({operator, _, 3, Operands}) -> lists:max(lists:map(fun eval/1, Operands));
eval({operator, _, 5, [A,B]}) ->
    AA = eval(A), BB = eval(B),
    if AA > BB -> 1; true -> 0 end;
eval({operator, _, 6, [A,B]}) ->
    AA = eval(A), BB = eval(B),
    if AA < BB -> 1; true -> 0 end;
eval({operator, _, 7, [A,B]}) ->
    AA = eval(A), BB = eval(B),
    if AA =:= BB -> 1; true -> 0 end.

-spec parse_packet(bitstring()) -> {packet(), bitstring()}.
parse_packet(<<Ver:3, 4:3, Groups/bits>>) ->
    {Val, Rest} = parse_group(Groups, 0),
    {{literal, Ver, Val}, Rest};
parse_packet(<<Ver: 3, Type:3, 0:1, Length:15, Operands/bits>>) ->
    {Packets, Rest} = parse_operands_len(Length, Operands, []),
    {{operator, Ver, Type, Packets}, Rest};
parse_packet(<<Ver: 3, Type:3, 1:1, SubPacketCnt:11, Operands/bits>>) ->
    {Packets, Rest} = parse_operands_cnt(SubPacketCnt, Operands, []),
    {{operator, Ver, Type, Packets}, Rest}.

-spec parse_group(bitstring(), uint()) ->
    {uint(), bitstring()}.
parse_group(<<0:1, Group:4, Rest/bits>>, Acc) ->
    {Acc bsl 4 + Group, Rest};
parse_group(<<1:1, Group:4, Rest/bits>>, Acc) ->
    parse_group(Rest, Acc bsl 4 + Group).

-spec parse_operands_cnt(uint(), bitstring(), [packet()]) ->
    {[packet()], bitstring()}.
parse_operands_cnt(0  , Bits, Acc) ->
    {Acc, Bits};
parse_operands_cnt(Cnt, Bits, Acc) ->
    {Packet, Rest} = parse_packet(Bits),
    parse_operands_cnt(Cnt-1, Rest, Acc ++ [Packet]).

-spec parse_operands_len(uint(), bitstring(), [packet()]) ->
    {[packet()], bitstring()}.
parse_operands_len(0  , Bits, Acc) ->
    {Acc, Bits};
parse_operands_len(Len, Bits, Acc) ->
    {Packet, Rest} = parse_packet(Bits),
    Remaining = Len-(bit_size(Bits) - bit_size(Rest)),
    parse_operands_len(Remaining, Rest, Acc ++ [Packet]).
