-module(alignment).
-export([compare/0, create_matrix/2]).

-type matrix() :: [[any(), ...]].

compare() ->
    {ok, [DNA_A]} = io:fread("First DNA sequence: ", "~s"),
    {ok, [DNA_B]} = io:fread("Second DNA sequence: ", "~s").

-spec new(pos_integer(), pos_integer(), 
    fun((pos_integer(), pos_integer(), pos_integer(), pos_integer()) -> any())) -> matrix().
new(N, M, Generator) ->
    [[Generator(C, R, N, M) || C <- lists:seq(1, N)] || R <- lists:seq(1, M)].


-spec create_matrix(pos_integer(), pos_integer()) -> matrix().
create_matrix(N, M) ->
    new(N, M, fun(C, R, CS, _) -> CS * (R - 1) + C end).


