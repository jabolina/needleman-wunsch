-module(alignment).
-export([compare/0]).

-spec compare() -> matrix:matrix().
compare() ->
    {ok, [SequenceA]} = io:fread("First DNA sequence: ", "~s"),
    {ok, [SequenceB]} = io:fread("Second DNA sequence: ", "~s"),
    needleman_wunsch(SequenceA, SequenceB).

-spec similarity(string(), string()) -> integer().
similarity(FirstGene, SecondGene) ->
    case (string:equal(FirstGene, SecondGene, true)) of
        true -> 1;
        false -> -1
    end.

-spec needleman_wunsch(string(), string()) -> matrix:matrix().
needleman_wunsch(SequenceA, SequenceB) ->
    string_iterate(SequenceA,
    fun(A, FIdx) ->
        string_iterate(SequenceB,
        fun(B, SIdx) ->
                matrix:set_element(FIdx, SIdx,
                max(matrix:get_element(FIdx - 1, SIdx - 1, B)
                    + similarity(string:substr(SequenceA, FIdx - 1, 1),
                                string:substr(SequenceB, SIdx - 1, 1)),
                    max(matrix:get_element(FIdx - 1, SIdx, B) + (-1.0),
                        matrix:get_element(FIdx, SIdx - 1, B) + (-1.0))),
                B)
        end, A, 2)
    end, matrix:create_matrix(string:length(SequenceA) + 1, string:length(SequenceB) + 1), 2).

-spec string_iterate(list(), function(), matrix:matrix(), integer()) -> any().
string_iterate([_|T], Function, Matrix, Index) ->
    Modified = Function(Matrix, Index),
    string_iterate(T, Function, Modified, Index + 1);
string_iterate([], _, Matrix, _) -> Matrix.

