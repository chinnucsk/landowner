-module(protocol_package).

-export([
		txt_cmd/2
		]).

txt_cmd(Pid, <<"LUCAS-CONNECT START">>)->
	s_account:new_player(Pid).

