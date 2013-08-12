landowner
=========

Landowner is a puke game online.

Landowner includes server (using cowboy) and clients (directory /index), the server and clients communicates using protocol defined by myself with websocket.

The server develop use cowboy websocket, it can handle multiple players requests, the game server  will start a new game when players sastisfy 3, it support reconnect.

The client develop on web using js, The client send requests to the server and get corresponding user info and datas info, the js handle these datas, for exampole show photos, pukes, id .

Instrustion
-------
The module s_game_server s_account are the gen_server for creating and saving new game and connect new players.

The module s_player ,s_game are used for the detailed player and game informations.

The index directory contains css ,js, pic, view.

Goals
--------

Landowner aims to the following:

* Stable, fast online server.
* The big online erlang landowner game.

Configure
--------

If you want to define port or some other property, you can modify /priv/app.config.

If you want to defint the path of url, you can modify /priv/dispatch.script.

The file /include/landowner.hrl define the gameinfo, playerinfo record.

Getting Started
--------

* make && ./start.sh
* cd /index/view && open game.html && click connect
* click the puke you will send and click enter

TODO
--------
Now the connectting and the get userinfo have already finished, the rest is the logic of pukes, another thing is to optimize the client interface to have a better user Experience.

Contributions
-------
Welcome you guys to join me to develop a better game.
