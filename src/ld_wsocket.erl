%%% Copyright(c)
%%%
%%% Author lucas@yun.io
-module(ld_wsocket).
-author('lucas@yun.io').
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

%% Cowboy http callbacks
-export([init/3, handle/2, terminate/3]).

%% Cowboy ws callbacks
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-include("landowner.hrl").

-record(state, {player_pid=none :: node|pid()}).

-define(TIMEOUT,infinity).

%%% --------------------------------------------------------------------------

%% @doc Upgrade wsocket failed,init function.
init({_Any, http}, _Req, []) ->
    {upgrade, protocol, cowboy_websocket}.

%% @doc Upgrade wsocket failed,handle function.
handle(Req, State) ->
    {ok, Req, State}.

%% @doc Upgrade wsocket failed,terminate function.
terminate(_Reason, _Req, _State) ->
    ok.

%%% --------------------------------------------------------------------------

%% @doc Upgrade wsocket success,init function.
websocket_init(_Any, Req, []) ->
	?LOG("init over ~w~n",[self()]),
    {ok, Req, #state{player_pid=none},?TIMEOUT}.

%% @doc Upgrade wsocket success,init function.
%% @doc This function receive wscoket message,and handle the message, finally return the result to the client.
websocket_handle({text, <<"CONNECT">>}, Req, #state{player_pid=none}=State) ->
	?LOG("process connect message ~n", []),
	case s_account:new_player() of
		{ok, Pid, <<PlayerId:36/binary>>} when is_pid(Pid) ->
			reply(<<"WELCOME ", PlayerId/binary>>, Req, State#state{player_pid=Pid});
		R ->
			?LOG("Bad reply ~w", [R]),
			reply(<<"INTERNAL_ERROR">>, Req, State)
	end;
websocket_handle({text, <<"CONNECT AS ", PlayerId:36/binary>>}, Req, State) ->
    ?LOG("Processing CONNECT AS message ~p~n", [PlayerId]),
    s_account:do_register(self(), PlayerId),
	receive
		{match, P1, P2, P3} ->
			[R1,R2] = remove_self([P1,P2,P3],PlayerId,[]),
			Cmd = protocol_package:package(match, {R1, R2})
	end,
	reply(Cmd, Req, State);
websocket_handle({text, Data}, Req, #state{player_pid=Pid}=State) when is_pid(Pid) -> 
	?LOG("receive data ~p~n", [Data]),
	Res1 = protocol_package:txt_cmd(Data),
	Res = protocol_package:package(Res1),
	reply(Res, Req, State);
websocket_handle({text, Msg}, Req, State) ->
    ?LOG("Unexpected message ~w with state ~w ~n", [Msg, State]),
	{reply, {text, Msg}, Req, State }.

reply(Msg, Req, State) ->
    ?LOG("Sending message ~s~n", [Msg]),
    {reply, {text, Msg}, Req, State}.

%% @doc Upgrade wsocket success,info function.
%% @doc This function receive wscoket notice infos,and handle the message, finally return the result to the client.
websocket_info({send_pukes, Data}, Req, State) ->
	?LOG("websocket_info ~p~n", [Data]),
	Res = protocol_package:package({do_receive, Data}),
	reply(Res, Req, State);
websocket_info(shutdown, Req, State) ->
	?LOG("shutdown ~n",[]),
    {shutdown, Req, State}.

%% @doc Upgrade wsocket success,terminate function.
%% @doc This function terminate the connections.
websocket_terminate(Reason, _Req, _State) ->
	?LOG("websocket terminate ~p~n",[Reason]),
    ok.

%%% -----------------------------------------------------------------------
%%% private function
%%% -----------------------------------------------------------------------

%% @doc remove_self is used to remove the ID self from the given player list. 
-spec remove_self(list(),list(),list()) -> list().
remove_self([],_,Acc) ->
	lists:reverse(Acc);
remove_self([A|Rest],PlayerId,Acc) when A =/= PlayerId ->
	remove_self(Rest,PlayerId,[A|Acc]);
remove_self([_A|Rest],PlayerId,Acc) ->
	remove_self(Rest,PlayerId,Acc).
