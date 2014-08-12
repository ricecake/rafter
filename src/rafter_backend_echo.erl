-module(rafter_backend_echo).

-behaviour(rafter_backend).

%% Rafter backend callbacks
-export([init/1, read/2, write/2, stop/1]).

init(_) ->
    {ok, undefined}.

read(Command, State) ->
    {{ok, Command}, State}.

write(Command, State) ->
    {{ok, Command}, State}.

stop(_State) ->
    ok.
