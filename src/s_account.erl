-module(s_account).
-behaviour(gen_server).

-export([
		start_link/0,
		new_player/0
		]).

-export([
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
		]).

-include("landowner.hrl").

-record(state, {parent}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, self(), []).

new_player() ->
	gen_server:call(?MODULE, new_player).

%%% =================================================================
%%% internal
%%% =================================================================

init(ParentId) ->
	process_flag(trap_exit, true),
	ets:new(landowner_player_id_tbl, [named_table, private, set]),
	ets:new(landowner_player_pid_tbl, [named_table, private, set]),
	ets:new(landowner_player_notify_tbl, [named_table, private, set]),
	{ok, #state{parent=ParentId}}.

terminate(_Reason, _State) ->
	ets:delete(landowner_player_id_tbl),
	ets:delete(landowner_player_pid_tbl),
	ets:delete(landowner_player_notify_tbl),
	ok.

handle_call(new_player, From, State) ->
	?LOG("new player ~n", []),
	new_player(From, State).

handle_info({'EXIT', Pid, _Reason}, #state{parent=Pid} = State) ->
	{stop, parent_die, State};
handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
	?LOG("player ~w down cance seeks ,remove player", [Pid]),
	case ets:lookup(landowner_player_pid_tbl, Pid) of
		[] -> {noreply, State};
		[{Pid, PlayerId}] ->
			ets:delete(landowner_player_pid_tbl, Pid),
			ets:delete(landowner_player_id_tbl, PlayerId),
			{noreply, State}
	end;
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    do_player_quit(Pid),
    {noreply, State};
handle_info({timeout, _Ref, log_state}, State) ->
    N = ets:info(c4_player_notify_tbl, size),
    P = ets:info(c4_player_pid_tbl, size),
    I = ets:info(c4_player_id_tbl, size),
    ?LOG("~w state:~nPlayer notify = ~w~nPids = ~w~nIds = ~w~n", [?MODULE, N, P, I]),
    {noreply, State};
handle_info(Event,  State) ->
    unexpected(Event, State).
		
handle_cast(_, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


new_player({ParentPid, _Tag} = _From, State) ->
	{ok, Pid} = landowner_player:start_link(ParentPid),
	monitor(process, Pid),
	?LOG("start new player ~w~n",[Pid]),
	PlayerId = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
	ets:insert(landowner_player_id_tbl, {PlayerId, Pid}),
	ets:insert(landowner_player_pid_tbl, {Pid, PlayerId}),
	{reply, {ok, Pid, PlayerId}, State}.

do_register_player(Pid) ->
	ets:insert(landowner_player_notify_tbl, {Pid}).

do_player_quit(Pid) ->
    case ets:lookup(landowner_player_pid_tbl, Pid) of
        [{Pid, PlayerId}] ->
            ets:delete(c4_player_pid_tbl, Pid),
            ets:delete(c4_player_id_tbl, PlayerId),
            ok;
        [] -> no_player
    end.

unexpected(Event, State) ->
	?LOG("unexpected event ~w :~w",[Event, State]),
	{noreply, State}.
