-module(s_player).
-behaviour(gen_fsm).
% FSM callback exports
-export([start/1,start_link/1,init/1, first_init/3, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([
		do_first_init/3,
		my_turn/3,
		other_turn/3,
		other_turn1/3
		]).

-include("landowner.hrl").

% @doc Starts unsupervised c4_player process
start(Args) ->
	gen_fsm:start(?MODULE, Args, []).

% @doc Starts c4_player process supervised by and linked to current process
start_link(Args) ->
	gen_fsm:start_link(?MODULE, Args, []).

first_init(Pid, GameId,PlayerId) ->
	Players = s_game_server:find_playerid(GameId),
	gen_fsm:sync_send_event(Pid, {init,GameId, Players,PlayerId}).

% @doc Initializes state machine to the idle state
init([ParentId, PlayerId]) ->
	?LOG("Starting attached to caller ~w", [ParentId]),
	monitor(process, ParentId),
	{ok, do_first_init, #playerinfo{parent=ParentId, playerid=PlayerId}}.

do_first_init({init, GameId, Players,PlayerId}, _From, State) ->
	{ok, Puke, Senty} = s_game:first_init(GameId, Players,PlayerId),
	S = proplists:get_value(PlayerId, Senty),
	Next = is_farmer(S,GameId,PlayerId),
	Puke1 = get_more_pukes(Puke, S, GameId, PlayerId),
	Senty1 = is_landowner(Senty,[],[]),
	{reply, {ok,{Senty1,Puke1}}, Next, State}.

my_turn(_Event, _From, State) ->
	{nexstate,other_turn,State}.

other_turn(_Event, _From, State) ->
	{nexstate,other_turn1,State}.

other_turn1(_Event, _From, State) ->
	{nexstate,my_turn,State}.

handle_event(Event, StateName, Data) ->
	?LOG("Unexpected event in state ~w : ~w ~w", [StateName, Event, Data]),
	{ok, StateName, Data}.

% @doc Handles player disconnect, game quit and get state requests.
% Generic callback for synchronous events for all states.
handle_sync_event(_Event, _From, State, Data) ->
	{next_state, State, Data}.

% @doc Handles death of our caller process (user disconnection). 
% FSM miscellaneous event handling callback.
handle_info(Msg, StateName, Data) ->
	?LOG("Unexpected event ~w in state ~w : ~w", [Msg, StateName, Data]),
	{next_state, StateName, Data}.

% @doc Code hot swapping callback (does nothing now).
code_change(_OldVsn, StateName, Data, _Extra) ->
	{ok, StateName, Data}.

% @doc No real cleanup when player process dies.
terminate(_Reason, _StateName, _State) ->
	ok.


get_next(0) ->
	my_turn;
get_next(1) ->
	other_turn;
get_next(2) ->
	other_turn1;
get_next(_) ->
	?LOG1("Get Next Error ~n").

get_more_pukes(Puke, S, _Gid, _PlayerId) when S  == <<"farmer">> ->
	Puke;
get_more_pukes(_Puke, S, Gid, PlayerId) when S  == <<"landowner">> ->
	{ok,Total} = s_game:add_landowner_puke(Gid,PlayerId),
	proplists:get_value(PlayerId,Total);
get_more_pukes(_, _, _, _) ->
	?LOG1("Get More Pukes errors ~n").

is_farmer(<<"farmer">>, GameId, PlayerId) ->
	{ok,Seq} = s_game:get_farmer_seq(GameId),
	Seq1 = proplists:get_value(PlayerId,Seq),
	get_next(Seq1);
is_farmer(<<"landowner">>, _, _) ->
	my_turn;
is_farmer(_,_,_) ->
	?LOG1("Is Farmer error~n").

is_landowner([],Acc1,Acc2) ->
	lists:append(Acc1,Acc2);
is_landowner([{PlayerId,<<"farmer">>}|Rest],Acc1,Acc2) ->
	is_landowner(Rest,Acc1,[PlayerId|Acc2]);
is_landowner([{PlayerId,<<"landowner">>}|Rest],Acc1,Acc2) ->
	is_landowner(Rest,[PlayerId|Acc1],Acc2).
