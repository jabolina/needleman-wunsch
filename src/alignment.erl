-module(alignment).
-export([compare/0]).

-spec compare() -> matrix:matrix().
compare() ->
    {ok, [SequenceA]} = io:fread("First DNA sequence: ", "~s"),
    {ok, [SequenceB]} = io:fread("Second DNA sequence: ", "~s"),
    needleman_wunsch(SequenceA, SequenceB).

-spec similarity(string(), string()) -> integer().
similarity(FirstGene, SecondGene) ->
    case (FirstGene == SecondGene) of
        true -> 1;
        false -> -1
    end.

-spec needleman_wunsch(string(), string()) -> matrix:matrix().
needleman_wunsch(SequenceA, SequenceB) ->
    M = matrix:create_matrix(string:length(SequenceA), string:length(SequenceB)),
    string_iterate(SequenceA,
    fun(A, FIdx) ->
        string_iterate(SequenceB,
        fun(B, SIdx) ->
            Value = matrix:get_element(FIdx, SIdx, B),
            Matrix = matrix:set_element(FIdx, SIdx, FIdx, B),
            {Value, Matrix}
        end, A, 2)
    end, M, 2).

-spec string_iterate(list(), function(), matrix:matrix(), integer()) -> any().
string_iterate([_|T], Function, Matrix, Index) ->
    {_, Modified} = Function(Matrix, Index),
    string_iterate(T, Function, Modified, Index + 1);
string_iterate([], Function, Matrix, Index) ->
    Function(Matrix, Index).

