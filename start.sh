#!/bin/sh
APP=landowner
ERL=erl
COOKIE=landowner
NODE_NAME=$APP@127.0.0.1
CONFIG=priv/app.config
DIRS="deps"

exec $ERL -pa ebin \
		-boot start_sasl \
		-name $NODE_NAME \
		-setcookie $COOKIE \
		-config $CONFIG \
		-env ERL_LIBS $DIRS \
		-s landowner
