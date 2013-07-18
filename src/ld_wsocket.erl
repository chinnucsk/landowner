-module(ld_wsocket).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

%% Cowboy http callbacks
-export([init/3, handle/2, terminate/3]).

%% Cowboy ws callbacks
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-include("landowner.hrl").

-record(state, {player_pid=none :: node|pid()}).

%% --------------------------------------------------------------------------

init({_Any, http}, _Req, []) ->
    {upgrade, protocol, cowboy_websocket}.

handle(Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% --------------------------------------------------------------------------

websocket_init(_Any, Req, []) ->
    {ok, Req, #state{player_pid=none}}.

websocket_handle({text, <<"CONNECT">>}, Req, #state{player_pid=none}=State) ->
	?LOG("process connect message ~n", []),
	case s_account:new_player() of
		{ok, Pid, <<PlayerId:36/binary>>} when is_pid(Pid) ->
			reply(<<"WELCOME ", PlayerId/binary>>, Req, State#state{player_pid=PlayerId});
		R ->
			?LOG("Bad reply ~w", [R]),
			reply(<<"INTERNAL_ERROR">>, Req, State)
	end;
websocket_handle({text, <<"LUCASCONNECT AS ", PlayerId:36/binary>>}, Req, #state{player_pid=none} = State) ->
    ?LOG("Processing CONNECT AS message", []),
    case c4_player_master:connect(PlayerId) of
        {ok, Pid, <<NewPlayerId:36/binary>>} when is_pid(Pid) ->
            ?LOG("New player id = ~s", [NewPlayerId]),
            reply(<<"WELCOME ", NewPlayerId/binary>>, Req, State#state{player_pid=Pid});
        _ ->
            reply(<<"INTERNAL_ERROR">>, Req, State)
    end;
websocket_handle({text, Data}, Req, #state{player_pid=Pid}=State) when is_pid(Pid) ->
	?LOG("receive data ~p~n", [Data]),
	reply(Data, Req, State);
websocket_handle({text, Msg}, Req, State) ->
    ?LOG("Unexpected message ~w with state ~w", [Msg, State]),
	{reply, {text, Msg}, Req, State }.

reply(Msg, Req, State) ->
    ?LOG("Sending message ~s", [Msg]),
    {reply, {text, Msg}, Req, State}.

websocket_info(shutdown, Req, State) ->
    {shutdown, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
