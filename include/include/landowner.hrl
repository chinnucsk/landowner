-define(LOG(A, B), io:format(A, B)).
-define(LOG1(A), io:format(A)).

-record(gameinfo, {
		total_num = 54,
		total = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,
				b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,
				c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,
				d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,
				e14,e15],
		player_p = [],
		senty = [],
		last_p = [],
		rest = [],
		sync = false,
		farmer_seq = []
	}).

-record(playerinfo,{
		playerid,
		parent,
		seq = 0 :: integer()
	}).
