-module(epsd).
-export([epsd/1]).

epsd(Path) ->
    read_header(file:read_file(Path)).

read_header({ok,Binary}) ->
    read_signature(binary:part(Binary, 0, 4)),
    read_version(binary:part(Binary, 4, 2)),
    read_reserved(binary:part(Binary, 6, 6)),
    read_channels(binary:part(Binary, 12, 2));
    %% Height
    %% Width
    %% Depth
    %% Color Mode

read_header({error, Reason}) ->
    io:format("Error: ~p", Reason).

%%---- Read Signature ----%%

%% Is a PSD
read_signature(<<"8BPS">>) ->
    io:format("Is PSD ~n... correct Signature"),
    {ok};

%% Not a PSD
read_signature(_) ->
    io:format("Not a PSD: Signature"),
    {error, "Not a PSD"}.

%%---- Read Version ----%%

read_version(<<0,1>>) ->
    io:format("~n... correct Version"),
    {ok};

read_version(_) ->
    {error, unkownVersion}.

%%---- Read Reserved ----%%

read_reserved(<<0,0,0,0,0,0>>) ->
    io:format("~n... Correct Reserved"),
    {ok};

read_reserved(_) ->
    {error, unkown}.

%%---- Read Channels ----%%

read_channels(Binary) ->
    [_ | Channels] = binary_to_list(Binary),
    io:format("~n... Nummber of Channels: ~p", Channels),
    {ok}.