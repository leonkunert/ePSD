-module(epsd_image_resources).
-export([read_resources/1]).

read_resources({ok, Binary}) ->
    io:format("~nStarting with Image resources..."),
    read_lenght(binary:part(Binary, 30, 4)).

read_lenght(Binary) ->
    <<Data:32/integer-unsigned-big>> = Binary,
    io:format("~n... Length of Resources: "++integer_to_list(Data)).