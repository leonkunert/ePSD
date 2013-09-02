-module(epsd_color_data).
-export([read_color_data/1]).

read_color_data(Binary) ->
    %% Calculating Length
    %% Data
    io:format("~n Starting with Color Mode Data Section ...").