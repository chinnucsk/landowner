-module(ld_account).

-export([
		init/3,
		handle/2,
		terminate/3
		]).

-include("landowner.hrl").

-record(state,{}).

init({_, http}, Req, _HandlerOpts) ->
	{ok, Req, #state{}}.

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

terminate(_Reason, _Req, _State) ->
	ok.
