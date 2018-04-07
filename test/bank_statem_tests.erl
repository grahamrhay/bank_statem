-module(bank_statem_tests).

-include_lib("eunit/include/eunit.hrl").

get_balance_test() ->
    From = self(),
    Balance = 123,
    Data = #{balance=>Balance},
    {keep_state, Data, [{reply, From, Balance}]} = bank_statem:open({call, From}, get_balance, Data).

close_test() ->
    From = self(),
    Data = #{},
    {next_state, closed, Data, [{reply, From, closed}]} = bank_statem:open({call, From}, close, Data).

reopen_test() ->
    From = self(),
    Data = #{},
    {next_state, open, Data, [{reply, From, open}]} = bank_statem:closed({call, From}, reopen, Data).

deposit_test() ->
    From = self(),
    {keep_state, #{balance:=200}, [{reply, From, deposit_made}]} = bank_statem:open({call, From}, {deposit, 100}, #{balance=>100}).
