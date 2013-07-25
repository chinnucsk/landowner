
document.write('<script type="text/javascript" src="../js/check.js"></script>');

var conn = new WebSocket('ws://localhost:8001/wsocket');

var sum = new Array();

var player_id = 0;

var total = new Array();

var senty = "farmer";

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
}

OpenConnection();

function click_pic(V)
{
	var value = V.className;
    if(value == "pic")
        value = "pic-top";
    else
        value = "pic";
    V.className=value;
	var p_value = get_value(V.id);
	total.push(p_value);
};

function get_value(V)
{
	alert("valuev "+V.substring(1));
	var  value = parseInt(V.substring(1));
	alert("value "+value);
	return value;
};

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

