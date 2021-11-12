%%%-------------------------------------------------------------------
%% @doc tlser public API
%% @end
%%%-------------------------------------------------------------------

-module(tlser_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tlser_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
