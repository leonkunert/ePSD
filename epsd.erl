-module(epsd).
-export([epsd/1, read_header/1, read_signature/1]).

epsd(Path) ->
    read_header(file:read_file(Path)).

read_header({ok,Binary}) ->
    read_signature(binary:part(Binary, 0, 4));
    %% Siganture
    %% Version
    %% Reserved
    %% Channels
    %% Height
    %% Width
    %% Depth
    %% Color Mode

read_header({error, Reason}) ->
    io:format("Error").

%% Is a PSD
read_signature("8BPS") ->
    io:format("Cool a PSD!");

%% Not a PSD
read_signature(_) ->
    io:format("Somthing went wrong.").