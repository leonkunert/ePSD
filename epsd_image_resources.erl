-module(epsd_image_resources).
-export([read_resources/1]).

read_resources({ok, Binary}) ->
    io:format("~nStarting with Image resources..."),
    read_lenght(binary:part(Binary, 30, 4)),
    read_signature(binary:part(Binary, 34, 4)),
    read_resource_id(binary:part(Binary, 38, 2)).

read_lenght(Binary) ->
    <<Data:32/integer-unsigned-big>> = Binary,
    io:format("~n... Length of Resources: "++integer_to_list(Data)),
    {ok, lenght_resources, Data, Binary}.

%%---- Read Signature ----%%

%% Is a PSD
read_signature(<<"8BIM">>) ->
    io:format("Is PSD ~n... correct Signature"),
    {ok, signature_resources};

%% Not a PSD
read_signature(_) ->
    io:format("Not a PSD: Signature"),
    {error, signature_resources, "Not a PSD"}.

%%---- Read Resource ID ----%%
read_resource_id(Binary) ->
    <<Data:16/integer-unsigned-big>> = Binary,
    if Data =:= 1000  ->
            %% (Obsolete--Photoshop 2.0 only ) Contains five 2-byte values: number of channels, rows, columns, depth, and mode
            io:format("~n... Obsolete (1000)");
       Data =:= 1001  ->
            %% Macintosh print manager print info record
            io:format("~n... Macintosh print manager print info record (1001)");
       Data =:= 1003  ->
            %% (Obsolete--Photoshop 2.0 only ) Indexed color table
            io:format("~n... Obsolete (1003)");
       Data =:= 1005  ->
            %% ResolutionInfo structure. See Appendix A in Photoshop API Guide.pdf.
            io:format("~n... ResolutionInfo structure (1005)");
       Data =:= 1006  ->
            %% Names of the alpha channels as a series of Pascal strings.
            io:format("~n... Names of the alpha channels as a series of Pascal strings. (1006)");
       Data =:= 1007  ->
            %% (Obsolete) See ID 1077DisplayInfo structure. See Appendix A in Photoshop API Guide.pdf.
            io:format("~n... Obsolete");
       Data =:= 1008  ->
            %% The caption as a Pascal string.
            io:format("~n... Pascal string caption");
       Data =:= 1009  ->
            %% Border information. Contains a fixed number (2 bytes real, 2 bytes fraction) for the border width, and 2 bytes for border units (1 = inches, 2 = cm, 3 = points, 4 = picas, 5 = columns).
            io:format("~n... Border Information");
       Data =:= 1010  ->
            %% Background color. See See Color structure.
            io:format("~n... Background Color");
       Data =:= 1011  ->
            %% Print flags. A series of one-byte boolean values (see Page Setup dialog): labels, crop marks, color bars, registration marks, negative, flip, interpolate, caption, print flags.
            io:format("~n... Print flags");
       Data =:= 1012 ->
            %% Grayscale and multichannel halftoning information
            io:format("~n... Grayscale and multichannel halftoning information");
       Data =:= 1013 ->
            %% Color halftoning information
            io:format("~n... Color halftoning information");
       Data =:= 1014 ->
            %% Duotone halftoning information
            io:format("~n... Duotone halftoning information");
       Data =:= 1015 ->
            %% Grayscale and multichannel transfer function
            io:format("~n... Grayscale and multichannel transfer function");
       Data =:= 1016 ->
            %% Color transfer functions
            io:format("~n... Color transfer functions");
       Data =:= 1017 ->
            %% Duotone transfer functions
            io:format("~n... Duotone transfer functions");
       Data =:= 1018  ->
            %% Duotone image information
            io:format("~n... Duotone image information");
       Data =:= 1019  ->
            %% Two bytes for the effective black and white values for the dot range
            io:format("~n... GrayScale");
       Data =:= 1020  ->
            %% (Obsolete)
            io:format("~n... Obsolete");
       Data =:= 1021  ->
            io:format("~n... RGBColor");
       Data =:= 1022  ->
            io:format("~n... CMYKColor");
       Data =:= 1023  ->
            io:format("~n... HSLColor");
       Data =:= 1024  ->
            io:format("~n... HSBColor");
       Data =:= 1025  ->
            io:format("~n... Multichannel");
       Data =:= 1026  ->
            io:format("~n... Duotone");
       Data =:= 1027  ->
            io:format("~n... LabColor");
       Data =:= 1028 ->
            %% IPTC-NAA record. Contains the File Info... information. See the documentation in the IPTC folder of the Documentation folder.
            io:format("~n... IPTC-NAA record. Contains the File Info... information. See the documentation in the IPTC folder of the Documentation folder.");
       Data =:= 1029 ->
            io:format("~n... RGB48");
       Data =:= 1030 ->
            io:format("~n... Lab48");
       Data =:= 1031 ->
            io:format("~n... CMYK64");
       Data =:= 1032 ->
            io:format("~n... DeepMultichannel");
       Data =:= 1033 ->
            io:format("~n... Duotone16");
       Data =:= 1034  ->
            io:format("~n... Bitmap");
       Data =:= 1035  ->
            io:format("~n... GrayScale");
       Data =:= 1036  ->
            io:format("~n... IndexedColor");
       Data =:= 1037  ->
            io:format("~n... RGBColor");
       Data =:= 1038  ->
            io:format("~n... CMYKColor");
       Data =:= 1039  ->
            io:format("~n... HSLColor");
       Data =:= 1040  ->
            io:format("~n... HSBColor");
       Data =:= 1041  ->
            io:format("~n... Multichannel");
       Data =:= 1042  ->
            io:format("~n... Duotone");
       Data =:= 1043  ->
            io:format("~n... LabColor");
       Data =:= 1044 ->
            io:format("~n... Gray16");
       Data =:= 1045 ->
            io:format("~n... RGB48");
       Data =:= 1046 ->
            io:format("~n... Lab48");
       Data =:= 1047 ->
            io:format("~n... CMYK64");
       Data =:= 1048 ->
            io:format("~n... DeepMultichannel");
       Data =:= 1049 ->
            io:format("~n... Duotone16");
       Data =:= 1050  ->
            io:format("~n... Bitmap");
       Data =:= 1051  ->
            io:format("~n... GrayScale");
       Data =:= 1052  ->
            io:format("~n... IndexedColor");
       Data =:= 1053  ->
            io:format("~n... RGBColor");
       Data =:= 1054  ->
            io:format("~n... CMYKColor");
       Data =:= 1057  ->
            io:format("~n... HSLColor");
       Data =:= 1058  ->
            io:format("~n... HSBColor");
       Data =:= 1059  ->
            io:format("~n... Multichannel");
       Data =:= 1060  ->
            io:format("~n... Duotone");
       Data =:= 1061  ->
            io:format("~n... LabColor");
       Data =:= 1062 ->
            io:format("~n... Gray16");
       Data =:= 1064 ->
            io:format("~n... RGB48");
       Data =:= 1065 ->
            io:format("~n... Lab48");
       Data =:= 1066 ->
            io:format("~n... CMYK64");
       Data =:= 1067 ->
            io:format("~n... DeepMultichannel");
       Data =:= 1069 ->
            io:format("~n... Duotone16");
       Data =:= 1070  ->
            io:format("~n... Multichannel");
       Data =:= 1071  ->
            io:format("~n... Duotone");
       Data =:= 1072  ->
            io:format("~n... LabColor");
       Data =:= 1073 ->
            io:format("~n... Gray16");
       Data =:= 1074 ->
            io:format("~n... RGB48");
       Data =:= 1075 ->
            io:format("~n... Lab48");
       Data =:= 1076 ->
            io:format("~n... CMYK64");
       Data =:= 1077 ->
            io:format("~n... DeepMultichannel");
       Data =:= 1078 ->
            io:format("~n... Duotone16");
       Data =:= 1080  ->
            io:format("~n... Bitmap");
       Data =:= 1082  ->
            io:format("~n... GrayScale");
       Data =:= 1083  ->
            io:format("~n... IndexedColor");
       Data =:= 1084  ->
            io:format("~n... RGBColor");
       Data =:= 1085  ->
            io:format("~n... CMYKColor");
       Data =:= 1086  ->
            io:format("~n... HSLColor");
       Data =:= 1087  ->
            io:format("~n... HSBColor");
       Data =:= 1088  ->
            io:format("~n... Multichannel");
       Data >= 2000 < 2997 ->
            io:format("~n... Duotone");
       Data =:= 2999  ->
            io:format("~n... LabColor");
       Data =:= 3000 ->
            io:format("~n... Gray16");
       Data >= 4000; =< 4999 ->
            io:format("~n... RGB48");
       Data =:= 7000 ->
            io:format("~n... Lab48");
       Data =:= 7001 ->
            io:format("~n... CMYK64");
       Data =:= 8000 ->
            io:format("~n... DeepMultichannel");
       Data =:= 10000 ->
            io:format("~n... Duotone16");
       Data < 1000; > 10000 ->
            io:format("~n... Unkown Resource ID")
    end,
    {ok, resource_id, Data}.