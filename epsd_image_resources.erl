-module(epsd_image_resources).
-export([epsd_image_resources/1]).

epsd_image_resources(Binary) ->
    io:format("~nStarting with Image resources..."),
    read_lenght(binary:part(Binary, 26, 4)).