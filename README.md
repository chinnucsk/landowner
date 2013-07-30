landowner
=========

Landowner is a puke game online.

Landowner includes server (using cowboy) and clients (directory /index), the server and clients communicates using protocol defined by myself with websocket.

The server develop use cowboy websocket, it can handle multiple players requests, the game server  will start a new game when players sastisfy 3, it support reconnect.

The client develop on web using js, The client send requests to the server and get corresponding user info and datas info, the js handle these datas, for exampole show photos, pukes, id .

Goals
--------

Landowner aims to the following:

* Stable, fast online server.
* The big online erlang landowner game.

Getting Started
--------

* make && ./start.sh
* cd /index/view && open game.html && click connect

TODO
--------
Now the connectting and the get userinfo have already finished, the rest is the logic of pukes, another thing is to optimize the client interface to have a better user Experience.

Contributions
-------
Welcome you guys to join me to develop a better game.
