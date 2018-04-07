#!/usr/bin/bash

docker run --name bank_statem -it --rm -v ${PWD}:/app -w /app  erlang _build/default/rel/default/bin/default console
