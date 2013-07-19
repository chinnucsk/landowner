
//document.write('<script type="text/javascript" src="./check.js"></script>');

var conn = new WebSocket('ws://localhost:8001/wsocket');

var sum = new Array();

var total = new Array();

function OpenConnection()
{
	conn.onopen = function(event)
	{
		document.getElementById("notice1").value="connecting to server ... ...";
	};

	conn.onclose = function(event)
	{
		alert("wsocket close");
	};

//	conn.close();
};

function connect()
{
	conn.send("CONNECT");

	conn.onmessage = function(event)
	{
		alert(event.data);
		document.getElementById("notice1").value=event.data;	
	};
}

OpenConnection();

function click_pic(V)
{
    var id = "d" + String(V);
    var pid = "p" + String(V);
    value = document.getElementById(id).className;
    if(value == "pic")
        value = "pic-top";
    else
        value = "pic";
    document.getElementById(id).className=value;
	var p_value = get_value(pid);
	total.push(p_value);
};

function get_value(pid)
{
	var pic = document.getElementById(pid).src;
	var name = pic.substring(pic.lastIndexOf("/")+1);
	return name.substring(0,name.lastIndexOf("."));
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

