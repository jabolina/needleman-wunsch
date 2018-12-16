-module(alignment).
-export([compare_input/0, compare_file/1]).

-spec compare_input() -> any().
compare_input() ->
    {ok, [SequenceA]} = io:fread("First DNA sequence: ", "~s"),
    {ok, [SequenceB]} = io:fread("Second DNA sequence: ", "~s"),
    Matrix = step_one(SequenceA, SequenceB),
    {A, B} = step_two(Matrix, SequenceA, SequenceB),
    {X, Y} = matrix:dimension(Matrix),
    {A, B, matrix:get_element(X, Y, Matrix)}.

-spec compare_file(string()) -> any().
compare_file(FileName) ->
    case util:read_file(FileName) of
        [] -> matrix:create_matrix(1, 1);
        [SeqA, SeqB] ->
            Matrix = step_one(SeqA, SeqB),
            {X, Y} = matrix:dimension(Matrix),
            {A, B} = step_two(Matrix, SeqA, SeqB),
            {A, B, matrix:get_element(X, Y, Matrix)}
        end.

-spec similarity(string(), string()) -> integer().
similarity(FirstGene, SecondGene) ->
    case (string:equal(FirstGene, SecondGene, true)) of
        true -> 1;
        false -> -1
    end.

-spec step_one(string(), string()) -> matrix:matrix().
step_one(SequenceA, SequenceB) ->
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

-spec step_two(matrix:matrix(), string(), string()) -> any().
step_two(Matrix, SeqA, SeqB) ->
    align(fun({A, B}, {FIdx, SIdx}) ->
            Score = matrix:get_element(FIdx, SIdx, Matrix),
            ScoreDiag = matrix:get_element(FIdx - 1, SIdx - 1, Matrix),
            ScoreUp = matrix:get_element(FIdx, SIdx - 1, Matrix),
            ScoreLeft = matrix:get_element(FIdx - 1, SIdx, Matrix),
            Similarity = similarity(string:substr(SeqA, FIdx - 1, 1), string:substr(SeqB, SIdx - 1, 1)),
            if
                (Score == (ScoreDiag + Similarity)) ->
                    {string:substr(SeqA, FIdx - 1, 1) ++ A,
                    string:substr(SeqB, SIdx - 1, 1) ++ B,
                    FIdx - 1,
                    SIdx - 1};
                (Score == (ScoreLeft + (-1.0))) ->
                    {string:substr(SeqA, FIdx - 1, 1) ++ A,
                    "-" ++ B,
                    FIdx - 1,
                    SIdx};
                (Score == (ScoreUp + (-1.0))) ->
                    {"-" ++ A,
                    string:substr(SeqB, SIdx - 1, 1) ++ B,
                    FIdx,
                    SIdx - 1};
                true -> {A, B, FIdx, SIdx}
            end
        end, {fun(Idx, A, B) ->
            fill_gap(Idx, A, B,
                fun(FIdx, FSeqA, FSeqB) ->
                    {string:substr(SeqA, FIdx - 1, 1) ++ FSeqA,
                    "-" ++ FSeqB,
                    FIdx - 1}
                end)
        end, fun(Idx, A, B) ->
            fill_gap(Idx, A, B,
                fun(FIdx, FSeqA, FSeqB) ->
                    {"-" ++ FSeqA,
                    string:substr(SeqB, FIdx - 1, 1) ++ FSeqB,
                    FIdx - 1}
                end)
       end}, {"", ""}, matrix:dimension(Matrix)).

-spec align(function(), tuple(), tuple(), tuple()) -> any().
align(_, {_, FEndFunction}, {A, B}, {1, Idx}) -> FEndFunction(Idx, A, B);
align(_, {SEndFunction, _}, {A, B}, {Idx, 1}) -> SEndFunction(Idx, A, B);
align(Function, EndFunctions, {A, B}, {FIdx, SIdx}) ->
    {NewA, NewB, NewFIdx, NewSIdx} = Function({A, B}, {FIdx, SIdx}),
    align(Function, EndFunctions, {NewA, NewB}, {NewFIdx, NewSIdx}).

-spec fill_gap(integer(), string(), string(), function()) -> any().
fill_gap(1, A, B, _) -> {A, B};
fill_gap(Idx, A, B, Function) ->
    {NewA, NewB, NewIdx} = Function(Idx, A, B),
    fill_gap(NewIdx, NewA, NewB, Function).

-spec string_iterate(list(), function(), matrix:matrix(), integer()) -> any().
string_iterate([_|T], Function, Matrix, Index) ->
    Modified = Function(Matrix, Index),
    string_iterate(T, Function, Modified, Index + 1);
string_iterate([], _, Matrix, _) -> Matrix.

