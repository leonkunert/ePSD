-module(epsd_header).
-export([read_header/1]).

read_header({ok,Binary}) ->
    read_signature(binary:part(Binary, 0, 4)),
    read_version(binary:part(Binary, 4, 2)),
    read_reserved(binary:part(Binary, 6, 6)),
    read_channels(binary:part(Binary, 12, 2)),
    read_height(binary:part(Binary, 14, 4)),
    read_width(binary:part(Binary, 18, 4)),
    read_depth(binary:part(Binary, 22, 2)),
    read_mode(binary:part(Binary, 24, 2));

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
    {error, signature, "Not a PSD"}.

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

%%---- Read Depth ----%%

read_depth(Binary) -> 
    <<Data:16/integer-unsigned-big>> = Binary,
    io:format("~n... Depth is: "++ integer_to_list(Data)),
    {ok, depth, Data}.

%%---- Read Color Mode ----%%
    
read_mode(Binary) ->
    <<Data:16/integer-unsigned-big>> = Binary,
    if Data =:= 0  ->
            io:format("~n... Bitmap");
       Data =:= 1  ->
            io:format("~n... GrayScale");
       Data =:= 2  ->
            io:format("~n... IndexedColor");
       Data =:= 3  ->
            io:format("~n... RGBColor");
       Data =:= 4  ->
            io:format("~n... CMYKColor");
       Data =:= 5  ->
            io:format("~n... HSLColor");
       Data =:= 6  ->
            io:format("~n... HSBColor");
       Data =:= 7  ->
            io:format("~n... Multichannel");
       Data =:= 8  ->
            io:format("~n... Duotone");
       Data =:= 9  ->
            io:format("~n... LabColor");
       Data =:= 10 ->
            io:format("~n... Gray16");
       Data =:= 11 ->
            io:format("~n... RGB48");
       Data =:= 12 ->
            io:format("~n... Lab48");
       Data =:= 13 ->
            io:format("~n... CMYK64");
       Data =:= 14 ->
            io:format("~n... DeepMultichannel");
       Data =:= 15 ->
            io:format("~n... Duotone16");
       Data  >  15 ->
            io:format("~n... Unkown Color Mode")
    end.