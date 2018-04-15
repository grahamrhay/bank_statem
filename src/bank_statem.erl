-module(bank_statem).

-behaviour(gen_statem).

-export([start/0, stop/0, get_balance/0, close/0, reopen/0, deposit/1, withdraw/1]).
-export([init/1, callback_mode/0]).
-export([handle_event/4]).

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

withdraw(Amount) ->
    gen_statem:call(name(), {withdraw, Amount}).

stop() ->
    gen_statem:stop(name()).

init([]) ->
    {ok, open, #{balance=>0}}.

callback_mode() -> handle_event_function.

handle_event({call, From}, get_balance, open, #{balance:=Balance} = Data) ->
    {keep_state, Data, [{reply, From, Balance}]};

handle_event({call, From}, close, open, Data) ->
    {next_state, closed, Data, [{reply, From, closed}]};

handle_event({call, From}, {deposit, Amount}, open, #{balance:=Balance} = Data) when is_number(Amount) andalso Amount > 0 ->
    NewBalance = Balance + Amount,
    {keep_state, Data#{balance:=NewBalance}, [{reply, From, deposit_made}]};

handle_event({call, From}, {withdraw, Amount}, open, #{balance:=Balance} = Data) when is_number(Amount) andalso (Balance - Amount > 0) ->
    NewBalance = Balance - Amount,
    {keep_state, Data#{balance:=NewBalance}, [{reply, From, withdrawal_made}]};

handle_event({call, From}, reopen, closed, Data) ->
    {next_state, open, Data, [{reply, From, open}]}.
