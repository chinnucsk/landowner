%%% Copyright(c)
%%%
%%% Author lucas@yun.io
-module(s_player).
-author('lucas@yun.io').
-behaviour(gen_fsm).

%% FSM callback exports
-export([start/1,
		start_link/1,
		init/1, 
		first_init/3, 
		handle_event/3, 
		handle_sync_event/4, 
		handle_info/3, 
		terminate/3, 
		code_change/4
		]).

-export([
		do_first_init/3,
		my_turn/3,
		other_turn/3,
		other_turn1/3
		]).

-export([
		call_my_turn/4,
		call_other_turn/2
		]).

-include("landowner.hrl").

-define(TIMEOUT, infinity).

%% @doc start Start the game server.
-spec start(list()) -> {ok, pid()} | ignore | {error, binary()}.
start(Args) ->
	gen_fsm:start(?MODULE, Args, [{timeout,?TIMEOUT}]).

%% @doc start_link start the game server.
-spec start_link(list()) -> {ok, pid()} | ignore | {error, binary()}.
start_link(Args) ->
	gen_fsm:start_link(?MODULE, Args, [{timeout,?TIMEOUT}]).

%% @doc initialization a player.
first_init(Pid, GameId,PlayerId) ->
	Players = s_game_server:find_playerid(GameId),
	gen_fsm:sync_send_event(Pid, {init,GameId, Players,PlayerId}).

%% @doc Fsm my turn to send pukes.
call_my_turn(Pid, GameId, PlayerId, Pukes) ->
	gen_fsm:sync_send_event(Pid, {send_pukes, GameId, PlayerId, Pukes}).

%% @doc Fsm other trun to get other player pukes.
call_other_turn(Pid, GameId) ->
	gen_fsm:sync_send_event(Pid, {other_turn, GameId}, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([ParentId, PlayerId]) ->
	?LOG("Starting attached to caller ~w", [ParentId]),
	monitor(process, ParentId),
	{ok, do_first_init, #playerinfo{parent=ParentId, playerid=PlayerId}}.

do_first_init({init, GameId, Players,PlayerId}, _From, State) ->
	{ok, Puke, Senty, Seq} = s_game:first_init(GameId, Players,PlayerId),
	S = proplists:get_value(PlayerId, Senty),
%	Next = is_farmer(S,GameId,PlayerId),
	Puke1 = get_more_pukes(Puke, S, GameId, PlayerId),
	Senty1 = is_landowner(Senty,[],[]),
	{reply, {ok,{Senty1,Puke1,Seq}}, my_turn, State}.

my_turn({send_pukes, GameId, PlayerId, Pukes}, _From, State) ->
	ok = s_game:set_send_pukes(GameId, Pukes, PlayerId),
	{reply, ok, my_turn, State}.

other_turn({other_turn, GameId}, _From, State) ->
	{ok,Pukes} = s_game:get_other_turn_pukes(GameId),
	Next = case Pukes of
				failed ->
					other_turn;
				_ ->
					other_turn1
		   end,
	{reply, {ok, Pukes},Next,State}.

other_turn1({other_turn, GameId}, _From, State) ->
	{ok,Pukes} = s_game:get_other_turn_pukes(GameId),
	Next = case Pukes of
				failed ->
					other_turn;
				_ ->
					other_turn1
		   end,
	{reply,{ok,Pukes},Next,State}.

handle_event(Event, StateName, Data) ->
	?LOG("Unexpected event in state ~w : ~w ~w", [StateName, Event, Data]),
	{ok, StateName, Data}.

handle_sync_event(_Event, _From, State, Data) ->
	{next_state, State, Data}.

handle_info(Msg, StateName, Data) ->
	?LOG("Unexpected event ~w in state ~w : ~w", [Msg, StateName, Data]),
	{next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
	{ok, StateName, Data}.

terminate(_Reason, _StateName, _State) ->
	ok.

%% @doc get the status of the player.
get_next(0) ->
	my_turn;
get_next(1) ->
	other_turn;
get_next(2) ->
	other_turn1;
get_next(_) ->
	?LOG1("Get Next Error ~n").

%% @doc If the senty is landowner will get more pukes.
get_more_pukes(Puke, S, _Gid, _PlayerId) when S  == <<"farmer">> ->
	Puke;
get_more_pukes(_Puke, S, Gid, PlayerId) when S  == <<"landowner">> ->
	{ok,Total} = s_game:add_landowner_puke(Gid,PlayerId),
	proplists:get_value(PlayerId,Total);
get_more_pukes(_, _, _, _) ->
	?LOG1("Get More Pukes errors ~n").

%% @doc get the status.
is_farmer(<<"farmer">>, GameId, PlayerId) ->
	{ok,Seq} = s_game:get_farmer_seq(GameId),
	Seq1 = proplists:get_value(PlayerId,Seq),
	get_next(Seq1);
is_farmer(<<"landowner">>, _, _) ->
	my_turn;
is_farmer(_,_,_) ->
	?LOG1("Is Farmer error~n").

%%@doc return list without landowner.
is_landowner([],Acc1,Acc2) ->
	lists:append(Acc1,Acc2);
is_landowner([{PlayerId,<<"farmer">>}|Rest],Acc1,Acc2) ->
	is_landowner(Rest,Acc1,[PlayerId|Acc2]);
is_landowner([{PlayerId,<<"landowner">>}|Rest],Acc1,Acc2) ->
	is_landowner(Rest,[PlayerId|Acc1],Acc2).
