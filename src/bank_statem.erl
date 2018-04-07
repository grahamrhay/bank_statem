-module(bank_statem).

-behaviour(gen_statem).

-export([start/0, stop/0, get_balance/0, close/0]).
-export([init/1, callback_mode/0]).
-export([open/3]).

name() -> bank_statem.

start() ->
    gen_statem:start({local, name()}, ?MODULE, [], []).

get_balance() ->
    gen_statem:call(name(), get_balance).

close() ->
    gen_statem:call(name(), close).

stop() ->
    gen_statem:stop(name()).

init([]) ->
    {ok, open, #{balance=>0}}.

callback_mode() -> state_functions.

open({call, From}, get_balance, #{balance:=Balance} = Data) ->
    {keep_state, Data, [{reply, From, Balance}]};

open({call, From}, close, Data) ->
    {next_state, closed, Data, [{reply, From, closed}]}.
