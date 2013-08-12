%%% Copyright(c)
%%%
%%% Author lucas@yun.io
-module(s_game_server).
-author('lucas@yun.io').
-behaviour(gen_server).

-export([
		start_link/0,
		new_game/1,
		find_gameid/1,
		find_playerid/1
		]).

-export([
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
		]).

-define(TIME, 3).

-include("landowner.hrl").

-record(state, {parent}).

%% @doc start_link start the game server.
-spec start_link() -> {ok, pid()} | ignore | {error, binary()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, self(), []).

%% @doc new_game start a new game.
new_game([{P1,_},{P2,_},{P3,_}]) ->
	gen_server:call(?MODULE, {new_game,P1,P2,P3}).

%% @doc find_gameid find the gameid of the playerId.
find_gameid(PlayerId) ->
	gen_server:call(?MODULE, {find_gameid,PlayerId}).

%% @doc find the playerId in the some gameid.
find_playerid(Gid) ->
	gen_server:call(?MODULE, {find_playerid,Gid}).

%%% =================================================================
%%% internal
%%% =================================================================

init(ParentId) ->
	process_flag(trap_exit, true),
	ets:new(landowner_game_id_tbl, [named_table, private, set]),
	ets:new(landowner_game_pid_tbl, [named_table, private, set]),
	{ok, #state{parent=ParentId}}.

terminate(_Reason, _State) ->
	ets:delete(landowner_game_id_tbl),
	ets:delete(landowner_game_pid_tbl),
	ok.

handle_call({new_game,P1,P2,P3}, _From, State) ->
	?LOG("new game ~n", []),
	{ok,Gid} = s_game:start_link(),
	ets:insert(landowner_game_id_tbl, {Gid, {P1,P2,P3}}),
	ets:insert(landowner_game_pid_tbl, {P1, Gid}),
	ets:insert(landowner_game_pid_tbl, {P2, Gid}),
	ets:insert(landowner_game_pid_tbl, {P3, Gid}),
	{reply, ok, State};
handle_call({find_gameid, PlayerId}, _From ,State) ->
	[{_,Gid}] = ets:lookup(landowner_game_pid_tbl, PlayerId),
	{reply, Gid, State};
handle_call({find_playerid, Gid}, _From ,State) ->
	[{_,Players}] = ets:lookup(landowner_game_id_tbl, Gid),
	{reply, Players, State}.

handle_info({'EXIT', Pid, _Reason}, #state{parent=Pid} = State) ->
	{stop, parent_die, State};
handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
	?LOG("player ~w down cance seeks ,remove player", [Pid]),
	case ets:lookup(landowner_player_pid_tbl, Pid) of
		[] -> {noreply, State};
		[{_PlayerId, GameId}] ->
			[{_,{P1,P2,P3}}] = ets:lookup(landowner_game_id_tbl, GameId),
			ets:delete(landowner_game_id_tbl, GameId),
			ets:delete(landowner_game_pid_tbl, P1),
			ets:delete(landowner_game_pid_tbl, P2),
			ets:delete(landowner_game_pid_tbl, P3),
			{noreply, State}
	end;
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    {noreply, State};
handle_info(Event,  State) ->
    unexpected(Event, State).
		
handle_cast(_, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

unexpected(Event, State) ->
	?LOG("unexpected event ~w :~w",[Event, State]),
	{noreply, State}.

