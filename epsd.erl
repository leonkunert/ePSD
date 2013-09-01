-module(epsd).
-export([epsd/1]).

epsd(Path) ->
    epsd_header:read_header(file:read_file(Path)).