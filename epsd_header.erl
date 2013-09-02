-module(epsd_header).
-export([read_header/1]).

read_header({ok,Binary}) ->
    read_signature(binary:part(Binary, 0, 4)),
    read_version(binary:part(Binary, 4, 2)),
    read_reserved(binary:part(Binary, 6, 6)),
    read_channels(binary:part(Binary, 12, 2)),
    read_height(binary:part(Binary, 14, 4)),
    read_width(binary:part(Binary, 18, 4));
    %% Depth
    %% Color Mode

read_header({error, Reason}) ->
    io:format("Error: ~p", Reason).

%%---- Read Signature ----%%

%% Is a PSD
read_signature(<<"8BPS">>) ->
    io:format("Is PSD ~n... correct Signature"),
    {ok, signature};

%% Not a PSD
read_signature(_) ->
    io:format("Not a PSD: Signature"),
    {error, signature "Not a PSD"}.

%%---- Read Version ----%%

read_version(<<0,1>>) ->
    io:format("~n... correct Version"),
    {ok, version};

read_version(_) ->
    {error, version, unkownVersion}.

%%---- Read Reserved ----%%

read_reserved(<<0,0,0,0,0,0>>) ->
    io:format("~n... Correct Reserved"),
    {ok, reserved};

read_reserved(_) ->
    {error, reserved, unkown}.

%%---- Read Channels ----%%

read_channels(Binary) ->
    [_ | Channels] = binary_to_list(Binary),
    io:format("~n... Nummber of Channels: ~p", Channels),
    {ok, channels, Channels}.

%%---- Read Height ----%%

read_height(Binary) ->
    <<Data:32/integer-unsigned-big>> = Binary,
    io:format("~n... Height is: "++ integer_to_list(Data)++ " px"),
    {ok, heigth, Data}.

%%---- Read Width ----%%

read_width(Binary) ->
    <<Data:32/integer-unsigned-big>> = Binary,
    io:format("~n... Width is: "++ integer_to_list(Data)++ " px"),
    {ok, width, Data}.