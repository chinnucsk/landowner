
document.write('<script type="text/javascript" src="../js/check.js"></script>');

var conn = new WebSocket('ws://localhost:8001/wsocket');

var sum = new Array();

var player_id = 0;

var total = new Array();

var senty = "farmer";

//var last_puke = new Array();

var seq = 0;

function OpenConnection()
{
	conn.onopen = function(event)
	{
		document.getElementById("notice1").innerHTML="connecting to server ... ...";
	};

	conn.onclose = function(event)
	{
		alert("wsocket close");
		conn.close();
	};
};

function connect()
{
//	document.getElementById("connect").disabled="true";
	conn.send("CONNECT");

	conn.onmessage = function(event)
	{
		parse_connection(event.data);
		conn.send("CONNECT AS "+ player_id);
		conn.onmessage = function(event)
		{
			parse_match(event.data);
			first_init();
		}
	};
}

function first_init()
{
	conn.send("FIRST_INIT "+ player_id);
	conn.onmessage = function(event)
	{
		parse_init(event.data);
	};
	setTimeout("loop()",2000);
}

function loop()
{
	if (seq == 0)
	{
		alert("It's your turn !!!");
		seq = 2;
	}
	else if (seq == 1)
	{
		alert("xxx");
		receive_brocast();
		seq = 0;
	}
	else if(seq == 2) 
	{
		alert("yyy !!!");
		receive_brocast();
		seq = 1;
	}	
}

OpenConnection();

function click_pic(V)
{
	var value = V.className;
	var p_value = 0;
    if(value == "pic")
	{
        value = "pic-top";
		V.className=value;
//		p_value = get_value(V.id);
		p_value = V.id;
		total.push(p_value);
	}
    else
	{
    value = "pic";
    V.className=value;
//	p_value = get_value(V.id);
	p_value = V.id;
	delete_from_arrary(total, p_value);
	}
};

function get_value(V)
{
	var  value = parseInt(V.substring(1));
	return value;
};

function delete_from_array(total, value)
{
	for (var i=0; i<total.length; i++)
	{
		if(total[i] == value)
			total.splice(i,1);
	}
}

function handle()
{
	var data = check(total);
	if (data == "error") 
		alert("your choose are not correct");
	else
		{
		conn.send(data);

//		conn.onmessage = function(event)
//		{
//			document.getElementById("text1").value=event.data;	
//		}
		}
}

function send_pukes()
{
	var result = check(total);
	if (result != 0)
	{
		alert("your input error");
	}
	else
	{
		puke_string = connect_puke_string(); 
		conn.send("SEND_PUKES " + player_id + puke_string);
		conn.onmessage = function(event)
		{
			if (parse_send_pukes(event.data) == "ok")
			{
				decrease_sum();
				refreash_div_pukes();
				show_receive_result(total);	
				clean_total();
				loop();	
			}
			else 
			{
				alert("mayber server errors send pukes");
			}
		}
	}
}

function connect_puke_string()
{
	var strings = "";
	for (var i=0;i<total.length;i++)
	{
			strings = strings + " " + total[i];
	}
	return strings;
}

function clean_total()
{
	for (var i=0;i<total.length;i++)
	{
		total.pop();	
	}
}

function decrease_sum()
{
	for (var i=0;i<total.length;i++)
	{
		for(var j=0;j<sum.length;j++)
		{
			if(total[i] == sum[j])
				sum.splice(j,1);
		}
	}
}

function receive()
{
	conn.send("RECEIVE ");
	conn.onmessage = function(event)
	{
		alert(event.data);	
	};
}

function refreash_div_pukes()
{
	var html = "";
	for (var i=0; i<sum.length;i++)
    {
            html=html + "<div id=\"" + sum[i] + "\" class=\"pic\"><img src=\"../pic/" + sum[i] + ".jpg\" onclick=\"click_pic(" + sum[i] + ")\" /></div>";
    };
    document.getElementById("div_b_r").innerHTML = html;
}

function receive_brocast()
{
//	conn.send("RECEIVE " + player_id);
	conn.onmessage = function(event)
	{
		var receive_pukes = parse_receive(event.data);
		if (receive_pukes == "null")
		{
		alert("get message failed");
		setTimeout("receive_brocast()",3000);
		}
		else
		{
		alert("got the correct value"+receive_pukes);
		var puke_list = new Array();
		puke_list.push(receive_pukes);
		show_receive_result(puke_list);
		loop();
		return;
		}
	}
}

function show_receive_result(pukes)
{
	var html = "";
	for (var i=0; i<pukes.length;i++)
	{
		html = html + "<div id=\"" + pukes[i] + "\" class=\"pic\"><img src=\"../pic/"  + pukes[i] + ".jpg\" /></div>";
	};
    document.getElementById("div_m_t_s").innerHTML = html;
}
