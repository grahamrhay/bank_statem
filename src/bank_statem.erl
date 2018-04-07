-module(bank_statem).

-behaviour(gen_statem).

-export([start/0, stop/0, get_balance/0, close/0, reopen/0, deposit/1]).
-export([init/1, callback_mode/0]).
-export([open/3, closed/3]).

name() -> bank_statem.

start() ->
    gen_statem:start({local, name()}, ?MODULE, [], []).

get_balance() ->
    gen_statem:call(name(), get_balance).

close() ->
    gen_statem:call(name(), close).

reopen() ->
    gen_statem:call(name(), reopen).

deposit(Amount) ->
    gen_statem:call(name(), {deposit, Amount}).

stop() ->
    gen_statem:stop(name()).

init([]) ->
    {ok, open, #{balance=>0}}.

callback_mode() -> state_functions.

open({call, From}, get_balance, #{balance:=Balance} = Data) ->
    {keep_state, Data, [{reply, From, Balance}]};

open({call, From}, close, Data) ->
    {next_state, closed, Data, [{reply, From, closed}]};

open({call, From}, {deposit, Amount}, #{balance:=Balance} = Data) when is_number(Amount) andalso Amount > 0 ->
    NewBalance = Balance + Amount,
    {keep_state, Data#{balance:=NewBalance}, [{reply, From, deposit_made}]}.

closed({call, From}, reopen, Data) ->
    {next_state, open, Data, [{reply, From, open}]}.
