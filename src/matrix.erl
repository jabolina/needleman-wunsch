-module(matrix).
-export([create_matrix/2, set_element/4]).

-type matrix() :: [[any(), ...]].

-spec new(pos_integer(), pos_integer(),
    fun((pos_integer(), pos_integer(), pos_integer(), pos_integer()) -> any())) -> matrix().
new(ColumnsSize, RowsSize, Generator) ->
    [[Generator(Column, Row, ColumnsSize, RowsSize) || Column <- lists:seq(1, ColumnsSize)] || Row <- lists:seq(1, RowsSize)].

-spec create_matrix(pos_integer(), pos_integer()) -> matrix().
create_matrix(ColumnsSize, RowsSize) ->
    new(ColumnsSize, RowsSize, fun(Column, _, _, _) ->  Column - 1 end).

-spec dimension(matrix()) -> {pos_integer(), pos_integer()}.
dimension(Matrix) ->
    {length(lists:nth(1, Matrix)), length(Matrix)}.

-spec get_element(pos_integer(), pos_integer(), matrix()) -> any().
get_element(Column, Row, Matrix) ->
    lists:nth(Column, lists:nth(Row, Matrix)).

-spec set_element(pos_integer(), pos_integer(), any(), matrix()) -> matrix().
set_element(PosColumn, PosRow, Element, Matrix) ->
    {Width, Height} = dimension(Matrix),
    new(Width, Height, fun(Column, Row, _, _) ->
        case (Column == PosColumn) andalso (Row == PosRow) of
            true ->
                Element;
            false ->
                get_element(Column, Row, Matrix)
        end
    end).


