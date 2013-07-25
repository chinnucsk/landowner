-module(s_game).
-behaviour(gen_server).

-export([
		start_link/0,
		first_init/3,
		get_farmer_seq/1,
		get_senty/2,
		get_total/1,
		get_rest/1,
		add_landowner_puke/2
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

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

first_init(Pid, Players, PlayerId) ->
	gen_server:call(Pid, {first_init,Players,PlayerId}).

add_landowner_puke(Pid,PlayerId) ->
	gen_server:call(Pid, {add_landowner_puke,PlayerId}).

get_farmer_seq(Pid) ->
	gen_server:call(Pid, get_farmer_seqs).

get_senty(Pid, PlayerId) ->
	gen_server:call(Pid, {get_senty, PlayerId}).

get_total(Pid) ->
	gen_server:call(Pid, get_total).

get_rest(Pid) ->
	gen_server:call(Pid, get_rest).
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
	{reply, {ok,Puke,Senty}, State3};
handle_call({get_senty, PlayerId}, _From ,#gameinfo{senty=Senty} = State) ->
	Val = proplists:get_value(PlayerId,Senty),
	{reply, {ok, Val},State};
handle_call({add_landowner_puke,PlayerId}, _From ,#gameinfo{total=Total,player_p=PlayerP}=State) ->
	Pukes = proplists:get_value(PlayerId, PlayerP),
    Pukes1 = Pukes ++ Total,
    TPukes = proplists:delete(PlayerId, PlayerP),
    TPukes1 = TPukes ++ [{PlayerId,Pukes1}],
	{reply,{ok,TPukes1},State#gameinfo{player_p=TPukes1}};
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

set_farmer_seq(Senty,State) ->
	FList = get_farmer(Senty,[]),
	{{P1,_}, [{P2,_}], _} = do_random_pukes(FList,2),
	List = [{P1,1},{P2,2}],
	State#gameinfo{farmer_seq=List}.

get_farmer([],Acc) ->
	lists:reverse(Acc);
get_farmer([{_,<<"farmer">>} = Player|Rest],Acc) ->
	get_farmer(Rest,[Player|Acc]);
get_farmer([{_,<<"landowner">>}|Rest],Acc) ->
	get_farmer(Rest, Acc);
get_farmer([{_,_}|_Rest],_Acc) ->
	?LOG1("Get Farmer error ~n").

