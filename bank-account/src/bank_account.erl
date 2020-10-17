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

bank_operation(#bank_account{status = closed}, _) ->
    {error, account_closed};
bank_operation(BankAccount, Operation) ->
    {ok, Operation(BankAccount)}.

update_balance(BankAccount, Amount) ->
    Balance = BankAccount#bank_account.balance,
    BankAccount#bank_account{balance = Balance + Amount}.

deposit_amount(BankAccount, Amount) ->
    bank_operation(BankAccount,
                   fun (Account) -> update_balance(Account, Amount) end).

get_balance_operation(BankAccount) ->
    bank_operation(BankAccount,
                   fun (Account) -> Account#bank_account.balance end).

close_account(BankAccount) ->
    BankAccount#bank_account{status = closed}.

next_state({ok, NextState}, From, _) ->
    From ! {ok, NextState},
    bank_account_process(NextState);
next_state({error, Error}, From, BankAccount) ->
    From ! {error, Error},
    bank_account_process(BankAccount).

bank_account_process(BankAccount) ->
    receive
        {From, {deposit, Amount}} ->
            Result = deposit_amount(BankAccount, Amount),
            next_state(Result, From, BankAccount);
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

charge(_Pid, _Amount) -> undefined.

-spec close(pid()) -> number().

close(Pid) ->
    Pid ! {self(), {close}},
    receive
        {ok, Balance} -> Balance;
        Error -> Error
    end.

start_bank_account() ->
    bank_account_process(#bank_account{}).

create() -> spawn(fun start_bank_account/0).

-spec deposit(pid(), number()) -> void.

deposit(Pid, Amount) ->
    Pid ! {self(), {deposit, Amount}},
    receive
        {ok, Balance} -> Balance;
        Error -> Error
    end.

withdraw(_Pid, _Amount) -> undefined.
