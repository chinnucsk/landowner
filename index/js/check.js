function check(total)
{
	return 0;
}

function parse_connection(msg)
{
	if (msg.substr(0,7) == "WELCOME")
	{
		player_id = msg.substring(8);
		document.getElementById("info1").innerHTML=msg.substring(8);	
	}
	else
		alert("connection parse error");
}

function parse_match(msg) 
{
	if (msg.substr(0,5) == "MATCH")
	{
		var player_id1 = msg.substring(6,42);
		var player_id2 = msg.substring(42);
		document.getElementById("info2").innerHTML=player_id1;
		document.getElementById("info3").innerHTML=player_id2;
	}
	else {
		alert("match error");
	}
		
}

function parse_init(msg)
{
	if (msg.substr(0,8) == "INIT_RES")
	{
		var player_id1 = msg.substring(9,45);
		var player_id2 = msg.substring(45,81);
		var pos1 = msg.substring(81,82);
		var player_id3 = msg.substring(82,118);
		var pos2 = msg.substring(118,119);
		var pukes = msg.substring(119);
		if (player_id == player_id1) 
			{
				if(pos1 == "1") 
				{
				document.getElementById("info2").innerHTML=player_id3;
				document.getElementById("info3").innerHTML=player_id2;
				}
				else
				{
				document.getElementById("info2").innerHTML=player_id2;
				document.getElementById("info3").innerHTML=player_id3;
				}
			}
		else if (player_id == player_id2)
		{
			if (pos1 == "1")
			{
				seq = 1;
				document.getElementById("info2").innerHTML=player_id1;
				document.getElementById("info3").innerHTML=player_id3;
			}
			else
			{
				seq = 2;
				document.getElementById("info2").innerHTML=player_id3;
				document.getElementById("info3").innerHTML=player_id1;
			}
		}
		else if (player_id == player_id3)
		{
			if (pos1 == "1")
			{
				seq = 2;
				document.getElementById("info2").innerHTML=player_id1;
				document.getElementById("info3").innerHTML=player_id2;
			}
			else
			{
				seq = 1;
				document.getElementById("info2").innerHTML=player_id2;
				document.getElementById("info3").innerHTML=player_id1;
			}
		};
		set_photo(player_id1);
		set_senty(player_id1);
		set_pukes(pukes);
	}
	else 
	{
		alert("init error");
	}
}

function parse_send_pukes(msg)
{
	if (msg == "SEND_PUKES ok")	
		return "ok";
	else 
		return "error";
}

function parse_receive(msg)
{
	if (msg == "")
	{
		var pukes = "null" 	
	}
	else
	{
		if (msg.substr(0,7) == "RECEIVE")
		{
			var pukes = msg.substring(8);
		}
		else
		{
			alert("parse receive error");	
		}
	}
	return pukes;
}


function set_photo(player_id1)
{
	if(document.getElementById("info1").innerHTML == player_id1)
	{
		document.getElementById("photo1").src = "../pic/dizhu.jpg";
	};

	if(document.getElementById("info2").innerHTML == player_id1)
	{
		document.getElementById("photo2").src = "../pic/dizhu.jpg";
	};
	if(document.getElementById("info3").innerHTML == player_id1)
	{
		document.getElementById("photo3").src = "../pic/dizhu.jpg";
	};
}

function set_senty(player_id1)
{
	if(player_id1 == player_id)
	{
		senty = "landowner";	
	}
}

function set_pukes(pukes)
{
	var puke_list = pukes.split(",");
	var html = "";
	for (var i=0; i<puke_list.length;i++)
	{
			html=html + "<div id=\"" + puke_list[i] + "\" class=\"pic\"><img src=\"../pic/" + puke_list[i] + ".jpg\" onclick=\"click_pic(" + puke_list[i] + ")\" /></div>";
	};
	document.getElementById("div_b_r").innerHTML = html;
	for (var j=0; j<puke_list.length;j++)
	{
		sum.push(puke_list[j]);	
	}
}
