-module(landowner_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(APP, landowner).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, Sup} = landowner_sup:start_link(),
	setup_cowboy(),
	{ok, Sup}.

stop(_State) ->
    ok.

setup_cowboy() ->
	NbAcceptors = get_env(acceptors, 100),
	{ok,_} = cowboy:start_http(?APP, NbAcceptors, trans_opts(), proto_opts()).

get_env(Key, Default) ->
	case application:get_env(?APP, Key) of
		{ok, Value} -> 
					Value;
		undefined ->
					Default
	end.

trans_opts() ->
	{ok, Ip} = inet_parse:address(get_env(ip, "10.32.0.75")),
	[
	{port, get_env(port, 8081)},
	{ip, Ip},
	{max_connections, get_env(max_connections, 1024)},
	{backlog, get_env(backlog, 1024)}
	].

proto_opts() ->
	DispatchFile = get_env(dispatch_file, "priv/dispatch.script"),
	{ok, Dispatch} = file:script(DispatchFile),
	[
	{env, [{dispatch, cowboy_router:compile(Dispatch)}]},
	{timeout, get_env(timeout, 300000)}
	].
