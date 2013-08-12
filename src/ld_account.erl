%%% Copyright(c)
%%%
%%% Author lucas@yun.io
-module(ld_account).

-author('lucas@yun.io').

-export([
		init/3,
		handle/2,
		terminate/3
		]).

-include("landowner.hrl").

-record(state,{}).

%% @doc handler init function.
init({_, http}, Req, _HandlerOpts) ->
	{ok, Req, #state{}}.

%% @doc handler handle function.
handle(Req, _State) ->
	Body = "<html>
			<head></head>
			<body>
			<h1>wo cao </h1>
			<form>
			<input type=\"submit\">
			</form>
			</body>
			</html>",
	cowboy_req:reply(200,[],Body,Req).

%% @doc handler terminate function.
terminate(_Reason, _Req, _State) ->
	ok.
