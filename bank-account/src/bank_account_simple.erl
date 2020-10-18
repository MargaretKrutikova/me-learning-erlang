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

% ---- core logic ----

update_balance(Account = #bank_account{balance = B},
               Amount) ->
    Account#bank_account{balance = B + Amount}.

deposit_amount(#bank_account{status = closed}, _) ->
    {error, account_closed};
deposit_amount(BankAccount, Amount) ->
    UpdatedAccount = update_balance(BankAccount, Amount),
    {ok, {UpdatedAccount, Amount}}.

withdraw_amount(#bank_account{status = closed}, _) ->
    {error, account_closed};
withdraw_amount(Account = #bank_account{balance = B},
                Amount) ->
    Withdraw = min(B, Amount),
    {ok, {update_balance(Account, -Withdraw), Withdraw}}.

charge_amount(#bank_account{status = closed}, _) ->
    {error, account_closed};
charge_amount(#bank_account{balance = B}, Amount)
    when Amount > B ->
    {error, not_enough_money};
charge_amount(BankAccount, Amount) ->
    {ok, {update_balance(BankAccount, -Amount), Amount}}.

get_balance(#bank_account{status = closed}) ->
    {error, account_closed};
get_balance(BankAccount) ->
    {ok, BankAccount#bank_account.balance}.

close_account(BankAccount) ->
    {ok, {BankAccount#bank_account{status = closed}, 0}}.

open_account() -> #bank_account{}.

% ---- process ----

handle_deposit(BankAccount, Amount, From) ->
    case deposit_amount(BankAccount, Amount) of
        {ok, {NextState, _}} ->
            From ! ok,
            NextState;
        Error = {error, _} ->
            From ! Error,
            BankAccount
    end.

handle_withdraw(BankAccount, Amount, From) ->
    case withdraw_amount(BankAccount, Amount) of
        {ok, {NextState, AmountWithdrawn}} ->
            From ! {ok, AmountWithdrawn},
            NextState;
        Error = {error, _} ->
            From ! Error,
            BankAccount
    end.

handle_charge(BankAccount, Amount, From) ->
    case charge_amount(BankAccount, Amount) of
        {ok, {NextState, AmountCharged}} ->
            From ! {ok, AmountCharged},
            NextState;
        Error = {error, _} ->
            From ! Error,
            BankAccount
    end.

handle_account_close(BankAccount, From) ->
    {ok, {NextState, Balance}} = close_account(BankAccount),
    From ! {ok, Balance},
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

% validation logic

validate_amount(Amount) when Amount < 0 ->
    {error, invalid_amount};
validate_amount(_Amount) -> ok.

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
    spawn(fun () -> bank_account_process(open_account())
          end).

deposit(Pid, Amount) ->
    case validate_amount(Amount) of
        ok ->
            Pid ! {self(), {deposit, Amount}},
            receive Msg -> Msg end;
        {error, invalid_amount} -> 0
    end.

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

charge(Pid, Amount) ->
    case validate_amount(Amount) of
        ok ->
            Pid ! {self(), {charge, Amount}},
            receive
                {ok, AmountCharged} -> AmountCharged;
                {error, not_enough_money} -> 0;
                Error = {error, _} -> Error
            end;
        {error, invalid_amount} -> 0
    end.
