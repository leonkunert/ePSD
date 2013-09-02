-module(epsd_color_data).
-export([read_color_data/1]).

read_color_data({ok, Binary}) ->
    %% Calculating Length
    %% Data
    io:format("~nStarting with Color Data ..."),
    read_lenght(binary:part(Binary, 26, 4));

read_color_data({error, Reason}) ->
    io:format("Error: ~p", Reason).

%%---- Read Lenght ----%%

read_lenght(Binary) ->
    <<Data:32/integer-unsigned-big>> = Binary,
    if  Data =:= 0 ->
            io:format("~n... No Color Data found.");
        Data > 0 ->
            %% Still needs to deal with Color Data
            io:format("~n... Color Data length: "++ integer_to_list(Data))
    end.