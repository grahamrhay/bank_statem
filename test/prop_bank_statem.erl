-module(prop_bank_statem).

-include_lib("proper/include/proper.hrl").

-compile(export_all).

prop_test() ->
    ?FORALL(Cmds, proper_fsm:commands(?MODULE),
        ?TRAPEXIT(
            begin
                bank_statem:start_link(),
                {History,State,Result} = proper_fsm:run_commands(?MODULE, Cmds),
                bank_statem:stop(),
                ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n", [History,State,Result]),
                    aggregate(zip(proper_fsm:state_names(History), command_names(Cmds)), Result =:= ok))
            end)).

initial_state() -> open.

initial_state_data() -> #{balance=>0}.

open(_Data) ->
    [
        {open, {call, bank_statem, deposit, [pos_integer()]}},
        {open, {call, bank_statem, withdraw, [pos_integer()]}}
    ].

weight(_FromState, _ToState, _Call) -> 1.

precondition(_From, _To, #{balance:=Balance}, {call, bank_statem, withdraw, [Amount]}) ->
    Balance - Amount >= 0;

precondition(_From, _To, _Data, {call, _Mod, _Fun, _Args}) -> true.

postcondition(_From, _To, #{balance:=PrevBalance}, {call, bank_statem, deposit, [Amount]}, {deposit_made, UpdatedBalance}) ->
    UpdatedBalance =:= (PrevBalance + Amount);

postcondition(_From, _To, _Data, {call, _Mod, _Fun, _Args}, _Res) -> true.

next_state_data(_From, _To, #{balance:=Balance}=Data, _Res, {call, bank_statem, deposit, [Amount]}) ->
    NewBalance = Balance + Amount,
    Data#{balance:=NewBalance};

next_state_data(_From, _To, #{balance:=Balance}=Data, _Res, {call, bank_statem, withdraw, [Amount]}) ->
    NewBalance = Balance - Amount,
    Data#{balance:=NewBalance};

next_state_data(_From, _To, Data, _Res, {call, _Mod, _Fun, _Args}) ->
    NewData = Data,
    NewData.
