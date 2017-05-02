-module(3.10_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	3.10_sup:start_link().

stop(_State) ->
	ok.
