-module(bank_account_simple).

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

update_balance(Account = #bank_account{balance = B},
               Amount) ->
    Account#bank_account{balance = B + Amount}.

get_balance(#bank_account{status = closed}) ->
    {error, account_closed};
get_balance(BankAccount) ->
    {ok, BankAccount#bank_account.balance}.

handle_deposit(Account = #bank_account{status = closed},
               _, From) ->
    From ! {error, account_closed},
    Account;
handle_deposit(Account, Amount, From) ->
    UpdatedAccount = update_balance(Account, Amount),
    From ! ok,
    UpdatedAccount.

handle_withdraw(Account = #bank_account{status =
                                            closed},
                _, From) ->
    From ! {error, account_closed},
    Account;
handle_withdraw(Account = #bank_account{balance = B},
                Amount, From) ->
    Withdraw = min(B, Amount),
    From ! {ok, Withdraw},
    update_balance(Account, -Withdraw).

handle_charge(Account = #bank_account{status = closed},
              _, From) ->
    From ! {error, account_closed},
    Account;
handle_charge(Account = #bank_account{balance = B},
              Amount, From)
    when Amount > B ->
    From ! {ok, 0},
    Account;
handle_charge(BankAccount, Amount, From) ->
    From ! {ok, Amount},
    update_balance(BankAccount, -Amount).

handle_account_close(BankAccount, From) ->
    NextState = BankAccount#bank_account{status = closed},
    From ! {ok, 0},
    NextState.

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

balance(Pid) ->
    Pid ! {self(), {get_balance}},
    receive
        {ok, Balance} -> Balance;
        Error = {error, _} -> Error
    end.

close(Pid) ->
    Pid ! {self(), {close}},
    receive
        {ok, Balance} -> Balance;
        Error = {error, _} -> Error
    end.

create() ->
    spawn(fun () -> bank_account_process(#bank_account{})
          end).

deposit(_Pid, Amount) when Amount < 0 -> 0;
deposit(Pid, Amount) ->
    Pid ! {self(), {deposit, Amount}},
    receive Msg -> Msg end.

withdraw(_Pid, Amount) when Amount < 0 -> 0;
withdraw(Pid, Amount) ->
    Pid ! {self(), {withdraw, Amount}},
    receive
        {ok, AmountWithdrawn} -> AmountWithdrawn;
        Error = {error, _} -> Error
    end.

charge(_Pid, Amount) when Amount < 0 -> 0;
charge(Pid, Amount) ->
    Pid ! {self(), {charge, Amount}},
    receive
        {ok, AmountCharged} -> AmountCharged;
        Error = {error, _} -> Error
    end.
