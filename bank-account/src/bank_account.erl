-module(bank_account).

-import(bank_account_core,
        [charge_amount/2,
         close_account/1,
         deposit_amount/2,
         get_balance/1,
         open_account/0,
         withdraw_amount/2]).

-export([balance/1,
         charge/2,
         close/1,
         create/0,
         deposit/2,
         withdraw/2]).

-spec handle_deposit(bank_account_core:bank_account(),
                     number(), any()) -> bank_account_core:bank_account().

handle_deposit(BankAccount, Amount, From) ->
    case deposit_amount(BankAccount, Amount) of
        {ok, {NextState, _}} ->
            From ! ok,
            NextState;
        Error = {error, _} ->
            From ! Error,
            BankAccount
    end.

-spec handle_withdraw(bank_account_core:bank_account(),
                      number(), any()) -> bank_account_core:bank_account().

handle_withdraw(BankAccount, Amount, From) ->
    case withdraw_amount(BankAccount, Amount) of
        {ok, {NextState, AmountWithdrawn}} ->
            From ! {ok, AmountWithdrawn},
            NextState;
        Error = {error, _} ->
            From ! Error,
            BankAccount
    end.

-spec handle_charge(bank_account_core:bank_account(),
                    number(), any()) -> bank_account_core:bank_account().

handle_charge(BankAccount, Amount, From) ->
    case charge_amount(BankAccount, Amount) of
        {ok, {NextState, AmountCharged}} ->
            From ! {ok, AmountCharged},
            NextState;
        Error = {error, _} ->
            From ! Error,
            BankAccount
    end.

-spec
     handle_account_close(bank_account_core:bank_account(),
                          any()) -> bank_account_core:bank_account().

handle_account_close(BankAccount, From) ->
    {ok, {NextState, Balance}} = close_account(BankAccount),
    From ! {ok, Balance},
    NextState.

-spec
     bank_account_process(bank_account_core:bank_account()) -> ok.

bank_account_process(BankAccount) ->
    receive
        {From, {deposit, Amount}} ->
            NextState = handle_deposit(BankAccount, Amount, From),
            bank_account_process(NextState);
        {From, {withdraw, Amount}} ->
            NextState = handle_withdraw(BankAccount, Amount, From),
            bank_account_process(NextState);
        {From, {charge, Amount}} ->
            NextState = handle_charge(BankAccount, Amount, From),
            bank_account_process(NextState);
        {From, {close}} ->
            NextState = handle_account_close(BankAccount, From),
            bank_account_process(NextState);
        {From, {get_balance}} ->
            From ! get_balance(BankAccount),
            bank_account_process(BankAccount);
        terminate -> ok
    end.

% validation logic
validate_amount(Amount) when Amount < 0 ->
    {error, invalid_amount};
validate_amount(_Amount) -> ok.

-spec balance(pid()) -> number() |
                        {error, bank_account_core:bank_account_error()}.

balance(Pid) ->
    Pid ! {self(), {get_balance}},
    receive
        {ok, Balance} -> Balance;
        Error = {error, _} -> Error
    end.

-spec close(pid()) -> number() |
                      {error, bank_account_core:bank_account_error()}.

close(Pid) ->
    Pid ! {self(), {close}},
    receive
        {ok, Balance} -> Balance;
        Error = {error, _} -> Error
    end.

create() ->
    spawn(fun () -> bank_account_process(open_account())
          end).

-spec deposit(pid(), number()) -> number() |
                                  {error,
                                   bank_account_core:bank_account_error()}.

deposit(Pid, Amount) ->
    case validate_amount(Amount) of
        ok ->
            Pid ! {self(), {deposit, Amount}},
            receive Msg -> Msg end;
        {error, invalid_amount} -> 0
    end.

-spec withdraw(pid(), number()) -> number() |
                                   {error,
                                    bank_account_core:bank_account_error()}.

withdraw(Pid, Amount) ->
    case validate_amount(Amount) of
        ok ->
            Pid ! {self(), {withdraw, Amount}},
            receive
                {ok, AmountWithdrawn} -> AmountWithdrawn;
                {error, not_enough_money} -> 0;
                Error = {error, _} -> Error
            end;
        {error, invalid_amount} -> 0
    end.

-spec charge(pid(), number()) -> number() |
                                 {error,
                                  bank_account_core:bank_account_error()}.

charge(_Pid, Amount) when Amount < 0 -> 0;
charge(Pid, Amount) ->
    Pid ! {self(), {charge, Amount}},
    receive
        {ok, AmountCharged} -> AmountCharged;
        {error, not_enough_money} -> 0;
        Error = {error, _} -> Error
    end.
