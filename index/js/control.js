var total = new Array();

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
	var data = total.pop();
	conn.send(data);
}

function check(total)
{
	return ok;
}
