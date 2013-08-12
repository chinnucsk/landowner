%%% Copyright(c)
%%%
%%% Author lucas@yun.io
%%%
%%% @doc Convenience API to start landowner application. 
-module(landowner).

-author('lucas@yun.io').

-export([start/0]).

start() ->
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(?MODULE).
