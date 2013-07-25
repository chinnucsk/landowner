-module(s_account).
-behaviour(gen_server).

-export([
		start_link/0,
		new_player/0,
		do_register/2,
		find_pid/1
		]).

-export([
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
		]).

-define(TIME, 10000).

-include("landowner.hrl").

-record(state, {parent}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, self(), []).

new_player() ->
	gen_server:call(?MODULE, new_player).

do_register(Wid, PlayerId) ->
	gen_server:call(?MODULE, {register, Wid, PlayerId}).

find_pid(Pid) ->
	gen_server:call(?MODULE, {find_pid, Pid}).

%%% =================================================================
%%% internal
%%% =================================================================

init(ParentId) ->
	process_flag(trap_exit, true),
	ets:new(landowner_player_id_tbl, [named_table, public, set]),
	ets:new(landowner_player_pid_tbl, [named_table, public, set]),
	ets:new(landowner_player_notify_tbl, [named_table, public, set]),
	erlang:send_after(?TIME, self(), notify),
	{ok, #state{parent=ParentId}}.

terminate(_Reason, _State) ->
	ets:delete(landowner_player_id_tbl),
	ets:delete(landowner_player_pid_tbl),
	ets:delete(landowner_player_notify_tbl),
	ok.

handle_call(new_player, From, State) ->
	?LOG("new player ~n", []),
	new_player(From, State);
handle_call({register, Wid, PlayerId}, From, State) ->
	io:format("register ~p~p~n",[PlayerId,Wid]),
	do_register_player({PlayerId, Wid}),
	{reply, ok, State};
handle_call({find_pid, Playerid}, From ,State) ->
	[{_,Id}] = ets:lookup(landowner_player_id_tbl, Playerid),
	{reply, {ok, Id},State}.

handle_info(notify, State) ->
	Size = ets:info(landowner_player_notify_tbl,size),
	case Size >=  3 of
		false ->
			io:format("notiry....................~p~n",[Size]),
			erlang:send_after(?TIME, self(), notify),
			{noreply, State};
		true ->
			Res = check_notify(3,[]),
			s_game_server:new_game(Res),
			notify(Res),
			erlang:send_after(?TIME, self(), notify),
			{noreply, State}
	end;
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
	PlayerId = list_to_binary(uuid:uuid_to_string(uuid:get_v4())),
	{ok, Pid} = s_player:start_link([ParentPid,PlayerId]),
	monitor(process, Pid),
	?LOG("start new player ~w~n",[Pid]),
	ets:insert(landowner_player_id_tbl, {PlayerId, Pid}),
	ets:insert(landowner_player_pid_tbl, {Pid, PlayerId}),
	{reply, {ok, Pid, PlayerId}, State}.

do_register_player(Value) ->
	ets:insert(landowner_player_notify_tbl, Value),
	?LOG("start new player ~w~n",[ets:info(landowner_player_notiry_tbl,size)]).

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

check_notify(0, Acc) ->
	lists:reverse(Acc);
check_notify(N,Acc) ->
	Key1 = ets:last(landowner_player_notify_tbl),
	[A] = ets:lookup(landowner_player_notify_tbl, Key1),
	true = ets:delete(landowner_player_notify_tbl, Key1),
	check_notify(N-1,[A|Acc]).

notify([{P1,W1},{P2,W2},{P3,W3}]) ->
	W1 ! {match, P1, P2, P3},
	W2 ! {match, P1, P2, P3},
	W3 ! {match, P1, P2, P3}.

