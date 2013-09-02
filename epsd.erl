-module(epsd).
-export([epsd/1]).

epsd(Path) ->
    %% Caching File Data
    Binary = file:read_file(Path),
    %% Reading Header
    epsd_header:read_header(Binary),
    %% Reading Color Data
    epsd_color_data:read_color_data(Binary).