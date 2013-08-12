%%% Copyright(c)
%%%
%%% Author lucas@yun.io
-module(s_game).
-author('lucas@yun.io').
-behaviour(gen_server).

-export([
		start_link/0,
		first_init/3,
		get_farmer_seq/1,
		get_senty/2,
		get_total/1,
		get_rest/1,
		add_landowner_puke/2,
		set_send_pukes/3,
		get_other_turn_pukes/1
		]).

-export([
		init/1,
		handle_call/3,
		handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
		]).

-define(TIME, 1000).
-define(TIMEOUT, infinity).

-include("landowner.hrl").

%%% ---------------------------------------------------------------------
%%% Public Api
%%% ---------------------------------------------------------------------

%% @doc start_link start a new game pid.
-spec start_link() -> {ok, pid()} | ignore | {error, binary()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], [{timeout,?TIMEOUT}]).

%% @doc first_init The first init about the a new game.
first_init(Pid, Players, PlayerId) ->
	gen_server:call(Pid, {first_init,Players,PlayerId}).

%% @doc add_landowner_puke If is landowner,will add more three pukes.
add_landowner_puke(Pid,PlayerId) ->
	gen_server:call(Pid, {add_landowner_puke,PlayerId}).

%% @doc get_farmer_seq Get the sequnce of the farmer in the game.
get_farmer_seq(Pid) ->
	gen_server:call(Pid, get_farmer_seqs).

%% @doc get_senty Get the senty.
get_senty(Pid, PlayerId) ->
	gen_server:call(Pid, {get_senty, PlayerId}).

%% @doc get_total Get the total number of the pukes in the game.
get_total(Pid) ->
	gen_server:call(Pid, get_total).

%% @doc get_rest Get the rest pukes in the game.
get_rest(Pid) ->
	gen_server:call(Pid, get_rest).

%% @doc set_sen_pukes Sent pukes to the corresponding players.
set_send_pukes(Pid,Pukes,PlayerId) ->
	gen_server:call(Pid, {send_pukes, Pukes, PlayerId}).

%% @doc get_other_turn_pukes If other turn,will get the pukes sent by other player in the game.
get_other_turn_pukes(Pid) ->
	gen_server:call(Pid, get_pukes).

%%% =================================================================
%%% internal
%%% =================================================================

init([]) ->
	process_flag(trap_exit, true),
	{ok, #gameinfo{}}.

terminate(_Reason, _State) ->
	ok.

handle_call({first_init, {P1,P2,P3},PlayerId}, _From, State) ->
	?LOG("game frist init ~w ~n", [self()]),
	{Puke, State1} = get_puke(P1,P2,P3,PlayerId,State),	
	{Senty, State2} = get_senty(P1,P2,P3,State1),
	State3 = set_farmer_seq(Senty, State2),
	{reply, {ok,Puke,Senty,State3#gameinfo.farmer_seq}, State3};
handle_call({get_senty, PlayerId}, _From ,#gameinfo{senty=Senty} = State) ->
	Val = proplists:get_value(PlayerId,Senty),
	{reply, {ok, Val},State};
handle_call({add_landowner_puke,PlayerId}, _From ,#gameinfo{total=Total,player_p=PlayerP}=State) ->
	Pukes = proplists:get_value(PlayerId, PlayerP),
    Pukes1 = Pukes ++ Total,
    TPukes = proplists:delete(PlayerId, PlayerP),
    TPukes1 = TPukes ++ [{PlayerId,Pukes1}],
	{reply,{ok,TPukes1},State#gameinfo{player_p=TPukes1}};
handle_call({send_pukes,Pukes,PlayerId},_From,#gameinfo{total=Total,player_p=PlayerP,rest=Rest,total_num=TotalNum}=State) ->
	{NewTotal,N} = decrease_total_pukes(Total,Pukes,0),
	NowPukesPlayer = proplists:get_value(PlayerId,PlayerP),
	{NewPlayerP,_} = decrease_total_pukes(NowPukesPlayer, Pukes, 0),
	AtomPukes = atom_pukes(Pukes,[]),
	NewRest = lists:append(AtomPukes,Rest),
	State1 = State#gameinfo{total=NewTotal,player_p=NewPlayerP,total_num=TotalNum-N,rest=NewRest,last_p=AtomPukes},
	Players  = s_game_server:find_playerid(self()),
	{PS1,PS2} = get_other_player_wsocket(PlayerId,Players),
	io:format("s_game wsocket id ~p~p~n",[PS1,PS2]),
	PS1 ! {send_pukes, Pukes},
	PS2 ! {send_pukes, Pukes},
	{reply, ok, State1};
handle_call(get_pukes, _From, #gameinfo{last_p=LastP,sync=Sync} = State)->
	{Res,State2} = case LastP of
		[] ->
			{failed,State};
		_ ->
			 case Sync of
				false ->
					io:format("game server handle info lastap~p~n",[LastP]),
					{LastP,State};
				true ->
					{LastP,State#gameinfo{last_p=[],sync=false}}
			end
		end,
	{reply, {ok, Res},State2};
handle_call(get_total, _From ,#gameinfo{total=Total}=State) ->
	{reply, {ok, Total},State};
handle_call(get_rest, _From ,#gameinfo{rest=Rest}=State) ->
	{reply, {ok, Rest},State};
handle_call(get_farmer_seqs, _From, #gameinfo{farmer_seq=FarmerSeq} = State) ->
	{reply, {ok, FarmerSeq}, State}.
	

handle_info({'EXIT', _Pid, _Reason}, State) ->
	{stop, parent_die, State};
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

%% @doc get_puke This function use random algorithm to allot pukers to players.
-spec get_puke(list(),list(),list(),list(),#gameinfo{}) -> {list(),#gameinfo{}}.
get_puke(P1,P2,P3,PlayerId,#gameinfo{total=Total,player_p=PlayerP,total_num=TotalNum} = State) ->
	case PlayerP of
		[] ->
			[T,N,R1,R2,R3] = random_pukes(Total,TotalNum,[],[],[],18),
			PList = [{P1,R1},{P2,R2},{P3,R3}],
			Pukes = proplists:get_value(PlayerId,PList),
			State1 = #gameinfo{total=T, player_p=PList,total_num=N},
			{Pukes,State1};
		_ ->
			Pukes = proplists:get_value(PlayerId,PlayerP),
			{Pukes, State}
	end.

%% @doc random_pukes Random pukes .
random_pukes(Total,Num,C1,C2,C3, 1) ->
	[Total, Num, C1, C2, C3];
random_pukes(Total, TotalNum, C1, C2, C3, N) ->
	{V1, T1, N1} = do_random_pukes(Total, TotalNum),
	{V2, T2, N2} = do_random_pukes(T1, N1),
	{V3, T3, N3} = do_random_pukes(T2, N2),
	random_pukes(T3, N3, [V1|C1], [V2|C2], [V3|C3], N-1).
	

do_random_pukes(Total,TotalNum) ->
	Ran = random:uniform(TotalNum),
	V1 = lists:nth(Ran, Total),
	Total1 = lists:delete(V1,Total),
	{V1,Total1, TotalNum-1}.

%% @doc get_senty .This function to get senty in this game.
-spec get_senty(list(),list(),list(),#gameinfo{}) -> {list(),#gameinfo{}}.
get_senty(P1,P2,P3, #gameinfo{senty=Senty} = State) ->
	case Senty of
		[] ->
			Total = [<<"landowner">>,<<"farmer">>,<<"farmer">>],
			{V1, T1, N1} = do_random_pukes(Total, 3),
			{V2, T2, N2} = do_random_pukes(T1, N1),
			{V3, _T3, _N3} = do_random_pukes(T2, N2),
			Res = [{P1,V1},{P2,V2},{P3,V3}],
			State1 = State#gameinfo{senty=lists:flatten(Res)},
			{Res,State1};
		_ ->
			{Senty,State}
	end.

%% @doc set_farmer_seq Set the sequence of the farmers in this game.
-spec set_farmer_seq(list(),#gameinfo{}) ->#gameinfo{}.
set_farmer_seq(Senty,State) ->
	FList = get_farmer(Senty,[]),
	{{P1,_}, [{P2,_}], _} = do_random_pukes(FList,2),
	List = [{P1,1},{P2,2}],
	State#gameinfo{farmer_seq=List}.

%% @doc get_farmer Get farmers in this game.
-spec get_farmer(list(),list()) -> list().
get_farmer([],Acc) ->
	lists:reverse(Acc);
get_farmer([{_,<<"farmer">>} = Player|Rest],Acc) ->
	get_farmer(Rest,[Player|Acc]);
get_farmer([{_,<<"landowner">>}|Rest],Acc) ->
	get_farmer(Rest, Acc);
get_farmer([{_,_}|_Rest],_Acc) ->
	?LOG1("Get Farmer error ~n").

%% @doc decrease_total_pukes Decrease the pukes from total.
-spec decrease_total_pukes(list(), list(), integer()) ->
	{list(),integer()}.
decrease_total_pukes(Total, [], N) ->
	{Total, N};
decrease_total_pukes(Total, [A|Rest], N) ->
	Total1 = lists:delete(list_to_atom(A),Total),
	decrease_total_pukes(Total1, Rest, N+1).

%% @doc atom_pukes transfrom the pukes from lists to atom.
atom_pukes([], Acc) ->
	lists:reverse(Acc);
atom_pukes([A|Rest], Acc) ->
	atom_pukes(Rest, [list_to_atom(A)|Acc]).

%% @doc get_other_player_wsocket Get the wsocket pid in this game.
get_other_player_wsocket(PlayerId,{Player1,Player2,Player3}) ->
	[P1,P2] = get_other_player(PlayerId,[Player1,Player2,Player3],[]),
	io:format("get other player wsocket ~p~p~p~p~p~p",[PlayerId, Player1,Player2,Player3,P1,P2]),
	{ok,PS1} = s_account:find_wpid(P1),
	{ok,PS2} = s_account:find_wpid(P2),
	{PS1,PS2}.

%% @doc get_other_player Get the wsocket player in this game.
get_other_player(_PlayerId, [], Acc) ->
	lists:reverse(Acc);
get_other_player(PlayerId, [A|Rest], Acc) when PlayerId == A ->
	get_other_player(PlayerId, Rest, Acc );
get_other_player(PlayerId, [A|Rest], Acc) ->
	get_other_player(PlayerId, Rest, [A|Acc]).
