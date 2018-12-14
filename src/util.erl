-module(util).
-export([read_file/1]).

-spec read_file(string()) -> list().
read_file(FileName) ->
    case file:open(FileName, [read]) of
        {ok, Device} -> read_lines(Device);
        _ -> []
    end.

-spec read_lines(any()) -> list().
read_lines(Device) ->
    case file:read_line(Device) of
        {ok, Data} -> [string:trim(string:replace(Data, "\n", "")) | read_lines(Device)];
        eof -> []
    end.


