-module(protocol_package).

-export([
		txt_cmd/1,
		package/2,
		package/1,
		to_binary/1
		]).

txt_cmd(<<"FIRST_INIT ", PlayerId:36/binary>>)->
	{ok, Pid} = s_account:find_pid(PlayerId),
	Gid = s_game_server:find_gameid(PlayerId),
	{ok, {Senty, Pukes,Seq}} = s_player:first_init(Pid,Gid,PlayerId),
	{first_init,{Senty,Pukes,Seq}};
txt_cmd(<<"SEND_PUKES ",PlayerId:36/binary, Pukes/binary>>) ->
	{ok, Pid} = s_account:find_pid(PlayerId),
	Gid = s_game_server:find_gameid(PlayerId),
	PukeList = parse_pukes(binary_to_list(Pukes)),
	Res = s_player:call_my_turn(Pid, Gid, PlayerId, PukeList),
	{send_pukes, Res};
txt_cmd(<<"RECEIVE ", PlayerId:36/binary>>) ->
	{ok, Pid} = s_account:find_pid(PlayerId),
	Gid = s_game_server:find_gameid(PlayerId),
	{ok, Pukes} = s_player:call_other_turn(Pid, Gid),
	io:format("package protocal txt cmd receive ~p~n",[Pukes]),
	{do_receive, Pukes}.


package({first_init,{Senty,Pukes,Seq}}) ->
	[P1,P2,P3] = Senty,
	Pos2 = farmer_seq(P2, Seq),
	Pos3 = farmer_seq(P3, Seq),
	Pukes1 = to_list(Pukes,[]),
	Pukes2 = order_by_size(Pukes1, []), 
	<<"INIT_RES ", P1/binary, P2/binary, Pos2/binary, P3/binary, Pos3/binary, Pukes2/binary>>;
package({send_pukes, Res}) ->
	Res1 = list_to_binary(atom_to_list(Res)),
	<<"SEND_PUKES ", Res1/binary>>;
package({do_receive, failed}) ->
	<<"RECEIVE failed">>;
package({do_receive, Pukes}) ->
	io:format("package protocal do_receive ~p~n",[Pukes]),
	Pukes1 = order_by_size(Pukes,[]),
	<<"RECEIVE ",Pukes1/binary>>.

package(match, {P1,P2}) ->
	<<"MATCH ", P1/binary, P2/binary>>.

to_binary(A) when is_binary(A) ->
	A;
to_binary(A) when is_atom(A) ->
	erlang:atom_to_binary(A);
to_binary(A) when is_list(A) ->
	erlang:list_to_binary(A).

order_by_size([],Acc) ->
	Acc1 = add_delimiter(Acc,1,[]),
	list_to_binary(Acc1);
order_by_size([A|Rest],Acc) ->
	Min = compare(A,Rest),
	Rest1 = case Min of
				A ->
					Rest;
				_ ->
					List = lists:delete(Min,Rest),
					lists:append(List, [A])
			end,
	order_by_size(Rest1, [Min|Acc]).

compare(Min, []) ->
	Min;
compare(A, [B|Rest]) ->
	Min = do_compare(A, B),
	compare(Min, Rest).

do_compare([V1|Rest1] = A, [V2|Rest2] = B) ->
	case V1 == V2 of
		true ->
			case list_to_integer(Rest1) < list_to_integer(Rest2) of
				true ->
					A;
				false ->
					B
			end;
		false ->
			case V1 < V2 of
				true ->
					A;
				false ->
					B
			end
	end.

to_list([], Acc) ->
	Acc;
to_list([A|Rest],Acc) ->
	to_list(Rest,[atom_to_list(A)|Acc]).

add_delimiter([],_,Acc)->
	Acc;	
add_delimiter([A|Rest],N,Acc) ->
	case N == 1 of
		true ->
			add_delimiter(Rest,N+1,[A|Acc]);
		false ->
			add_delimiter(Rest,N+1,[A++","|Acc])
	end.

farmer_seq(_, []) ->
	error;
farmer_seq(A,[{B,Pos}|Rest1]) ->
	case A == B of
		true ->
			list_to_binary(integer_to_list(Pos));
		false ->
			farmer_seq(A, Rest1)
	end.

parse_pukes(Pukes) ->
	string:tokens(Pukes, " ").
