-module(bank_statem_tests).

-include_lib("eunit/include/eunit.hrl").

get_balance_test() ->
    From = self(),
    Balance = 123,
    Data = #{balance=>Balance},
    {keep_state, Data, [{reply, From, Balance}]} = bank_statem:handle_event({call, From}, get_balance, open, Data).

close_test() ->
    From = self(),
    Data = #{},
    {next_state, closed, Data, [{reply, From, closed}]} = bank_statem:handle_event({call, From}, close, open, Data).

reopen_test() ->
    From = self(),
    Data = #{},
    {next_state, open, Data, [{reply, From, open}]} = bank_statem:handle_event({call, From}, reopen, closed, Data).

deposit_test() ->
    From = self(),
    {keep_state, #{balance:=200}, [{reply, From, deposit_made}]} = bank_statem:handle_event({call, From}, {deposit, 100}, open, #{balance=>100}).

negative_deposit_test() ->
    From = self(),
    ?assertError(function_clause, bank_statem:handle_event({call, From}, {deposit, -1}, open, #{balance=>100})).

withdraw_test() ->
    From = self(),
    {keep_state, #{balance:=100}, [{reply, From, withdrawal_made}]} = bank_statem:handle_event({call, From}, {withdraw, 100}, open, #{balance=>200}).

negative_balance_test() ->
    From = self(),
    ?assertError(function_clause, bank_statem:handle_event({call, From}, {withdraw, 1}, open, #{balance=>0})).

place_hold_test() ->
    From = self(),
    Data = #{},
    {next_state, held, Data, [{reply, From, hold_placed}]} = bank_statem:handle_event({call, From}, place_hold, open, Data).

remove_hold_test() ->
    From = self(),
    Data = #{},
    {next_state, open, Data, [{reply, From, hold_removed}]} = bank_statem:handle_event({call, From}, remove_hold, held, Data).
