-module(epsd_image_resources).
-export([read_resources/1]).

read_resources({ok, Binary}) ->
    io:format("~nStarting with Image resources..."),
    read_lenght(binary:part(Binary, 30, 4)),
    read_signature(binary:part(Binary, 34, 4)),
    read_resource_id(binary:part(Binary, 38, 2)),
    read_name(binary:part(Binary, 40, 4)),
    read_actual_lenght(binary:part(Binary, 44, 4)),
    next_8_byte(binary:part(Binary, 0, 5000)).

read_lenght(Binary) ->
    <<Data:32/integer-unsigned-big>> = Binary,
    io:format("~n... Length of Resources: "++integer_to_list(Data)),
    {ok, lenght_resources, Data, Binary}.

%%---- Read Signature ----%%

%% Is a PSD
read_signature(<<"8BIM">>) ->
    io:format("~n... correct Signature"),
    {ok, signature_resources};

%% Not a PSD
read_signature(_) ->
    io:format("Not a PSD: Signature"),
    {error, signature_resources, "Not a PSD"}.

%%---- Read Resource ID ----%%
read_resource_id(Binary) ->
    <<Data:16/integer-unsigned-big>> = Binary,
    ResourceId = [1000,1001,1002,1003,1004,1005,1006,1007,1008,1009,
                  1010,1011,1012,1013,1014,1015,1016,1017,1018,1019,
                  1020,1021,1022,1023,1024,1025,1026,1027,1028,1029,
                  1030,1031,1032,1033,1034,1035,1036,1037,1038,1039,
                  1040,1041,1042,1043,1044,1045,1046,1047,1048,1049,
                  1050,1051,1052,1053,1054,1055,1056,1057,1058,1059,
                  1060,1061,1062,1063,1064,1065,1066,1067,1068,1069,
                  1070,1071,1072,1073,1074,1075,1076,1077,1078,1079,
                  1080,1081,1082,1083,1084,1085,1086,1087,1088,2999,
                  3000,7000,7001,8000,10000
                  ],
    io:format("~n... ResourceId: "++integer_to_list(Data)),
    lists:member(Data, ResourceId).


read_name(<<0,0,0,0>>) ->
    io:format("~n... empty Name."),
    {ok, name, ""};

read_name(Binary) ->
    Name = binary_to_list(Binary),
    io:format("~n... Name: "++Name),
    {ok, name, Name}.

read_actual_lenght(Binary) ->
    <<Data:32/integer-unsigned-big>> = Binary,
    Length = integer_to_list(Data),
    io:format("~n... Actual length of Resources: "++Length),
    {ok, actual_length_resources, Length}.

next_8_byte(Binary) ->
    io:format("~n... The next 8 bytes are: "++binary_to_list(Binary)).