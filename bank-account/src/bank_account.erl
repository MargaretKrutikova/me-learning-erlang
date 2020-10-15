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

-spec balance(bank_account) -> number().

balance(BankAccount) ->
    case BankAccount#bank_account.status of
        active -> BankAccount#bank_account.balance;
        closed -> erlang:error(account_closed)
    end.

charge(_Pid, _Amount) -> undefined.

-spec close(bank_account) -> number().

close(Pid) ->
    Balance = balance(Pid),
    _ = Pid#bank_account{status = closed},
    Balance.

-spec create() -> bank_account.

create() -> #bank_account{}.

-spec deposit(bank_account, number()) -> bank_account.

deposit(Pid, Amount) ->
    CurrentBalance = balance(Pid),
    Pid#bank_account{balance = CurrentBalance + Amount}.

withdraw(_Pid, _Amount) -> undefined.
