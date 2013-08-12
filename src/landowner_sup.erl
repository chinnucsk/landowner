%%% Copyright(c)
%%%
%%% Author lucas@yun.io
%%%
%%% @doc API to start supervesion.
-module(landowner_sup).

-author('lucas@yun.io').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Procs = procs([s_account,s_game_server], []),
    {ok, { {one_for_one, 5, 10}, Procs} }.

-spec procs([module()|{sup, module()}], [supervisor:child_spec()])
    -> [supervisor:child_spec()].
procs([], Acc) ->
    lists:reverse(Acc);
procs([{sup, Module}|Tail], Acc) ->
    procs(Tail, [sup(Module)|Acc]);
procs([Module|Tail], Acc) ->
    procs(Tail, [worker(Module)|Acc]).

-spec worker(M) -> {M, {M, start_link, []}, permanent, 5000, worker, dynamic}.
worker(Module) ->
    {Module, {Module, start_link, []}, permanent, 5000, worker, dynamic}.

-spec sup(M) -> {M, {M, start_link, []}, permanent, 5000, supervisor, [M]}.
sup(Module) ->
    {Module, {Module, start_link, []}, permanent, 5000, supervisor, [Module]}.

