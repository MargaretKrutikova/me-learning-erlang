-module(bank_account).

-export([balance/1,
         charge/2,
         close/1,
         create/0,
         deposit/2,
         withdraw/2]).

-type bank_account_status() :: active | closed.

-record(bank_account,
        {balance = 0 :: number(),
         status = active :: bank_account_status()}).

update_balance(BankAccount, Amount) ->
    Balance = BankAccount#bank_account.balance,
    BankAccount#bank_account{balance = Balance + Amount}.

deposit_amount(#bank_account{status = closed}, _) ->
    {error, account_closed};
deposit_amount(_BankAccount, Amount) when Amount < 0 ->
    {error, invalid_amount};
deposit_amount(BankAccount, Amount) ->
    {ok, update_balance(BankAccount, Amount)}.

withdraw_amount(#bank_account{status = closed}, _) ->
    {error, account_closed};
withdraw_amount(_BankAccount, Amount) when Amount < 0 ->
    {error, invalid_amount};
withdraw_amount(BankAccount, Amount)
    when Amount > BankAccount#bank_account.balance ->
    AmountToWithdraw = BankAccount#bank_account.balance,
    {ok,
     update_balance(BankAccount, -AmountToWithdraw),
     AmountToWithdraw};
withdraw_amount(BankAccount, Amount) ->
    {ok, update_balance(BankAccount, -Amount), Amount}.

charge_amount(#bank_account{status = closed}, _) ->
    {error, account_closed};
charge_amount(_BankAccount, Amount) when Amount < 0 ->
    {error, invalid_amount};
charge_amount(BankAccount, Amount)
    when Amount > BankAccount#bank_account.balance ->
    {ok, BankAccount, 0};
charge_amount(BankAccount, Amount) ->
    {ok, update_balance(BankAccount, -Amount), Amount}.

get_balance_operation(#bank_account{status = closed}) ->
    {error, account_closed};
get_balance_operation(BankAccount) ->
    {ok, BankAccount#bank_account.balance}.

close_account(BankAccount) ->
    BankAccount#bank_account{status = closed}.

%--------------------

bank_account_process(BankAccount) ->
    receive
        {From, {deposit, Amount}} ->
            case deposit_amount(BankAccount, Amount) of
                {ok, NextState} ->
                    From ! {ok},
                    bank_account_process(NextState);
                {error, Error} ->
                    From ! {error, Error},
                    bank_account_process(BankAccount)
            end;
        {From, {withdraw, Amount}} ->
            case withdraw_amount(BankAccount, Amount) of
                {ok, NextState, AmountWithdrawn} ->
                    From ! {ok, AmountWithdrawn},
                    bank_account_process(NextState);
                {error, Error} ->
                    From ! {error, Error},
                    bank_account_process(BankAccount)
            end;
        {From, {charge, Amount}} ->
            case charge_amount(BankAccount, Amount) of
                {ok, NextState, AmountCharged} ->
                    From ! {ok, AmountCharged},
                    bank_account_process(NextState);
                {error, Error} ->
                    From ! {error, Error},
                    bank_account_process(BankAccount)
            end;
        {From, {close}} ->
            NextState = close_account(BankAccount),
            From ! {ok, 0},
            bank_account_process(NextState);
        {From, {get_balance}} ->
            From ! get_balance_operation(BankAccount),
            bank_account_process(BankAccount);
        terminate -> ok
    end.

balance(Pid) ->
    Pid ! {self(), {get_balance}},
    receive
        {ok, Balance} -> Balance;
        Error -> Error
    end.

charge(Pid, Amount) ->
    Pid ! {self(), {charge, Amount}},
    receive
        {ok, AmountCharged} -> AmountCharged;
        {error, _} -> 0
    end.

close(Pid) ->
    Pid ! {self(), {close}},
    receive
        {ok, Balance} -> Balance;
        Error -> Error
    end.

start_bank_account() ->
    bank_account_process(#bank_account{}).

create() -> spawn(fun start_bank_account/0).

deposit(Pid, Amount) ->
    Pid ! {self(), {deposit, Amount}},
    receive Msg -> Msg end.

withdraw(Pid, Amount) ->
    Pid ! {self(), {withdraw, Amount}},
    receive
        {ok, AmountWithdrawn} -> AmountWithdrawn;
        {error, _} -> 0
    end.
