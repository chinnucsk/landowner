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
	{ok, {Senty, Pukes}} = s_player:first_init(Pid,Gid,PlayerId),
	{first_init,{Senty,Pukes}}.


package({first_init,{Senty,Pukes}}) ->
	[P1,P2,P3] = Senty,
	Pukes1 = to_list(Pukes,[]),
	Pukes2 = order_by_size(Pukes1, []), 
	<<"INIT_RES ", P1/binary, P2/binary, P3/binary, Pukes2/binary>>.

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
