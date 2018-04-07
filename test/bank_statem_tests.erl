-module(bank_statem_tests).

-include_lib("eunit/include/eunit.hrl").

get_balance_test() ->
    From = self(),
    Balance = 123,
    Data = #{balance=>Balance},
    {keep_state, Data, [{reply, From, Balance}]} = bank_statem:open({call, From}, get_balance, Data).
