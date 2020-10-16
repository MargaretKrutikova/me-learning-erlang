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

deposit_amount(BankAccount, Amount) ->
    Balance = BankAccount#bank_account.balance,
    BankAccount#bank_account{balance = Balance + Amount}.

get_account_balance(BankAccount) ->
    case BankAccount#bank_account.status of
        active -> {ok, BankAccount#bank_account.balance};
        closed -> {error, account_closed}
    end.

close_account(BankAccount) ->
    BankAccount#bank_account{status = closed}.

%---------------------

bank_account_process(BankAccount) ->
    receive
        {From, {deposit, Amount}} ->
            UpdatedBankAccount = deposit_amount(BankAccount,
                                                Amount),
            From ! {ok, UpdatedBankAccount},
            bank_account_process(UpdatedBankAccount);
        {From, {close}} ->
            UpdatedBankAccount = close_account(BankAccount),
            From ! {ok, 0},
            bank_account_process(UpdatedBankAccount);
        {From, {get_balance}} ->
            From ! get_account_balance(BankAccount),
            bank_account_process(BankAccount);
        terminate -> ok
    end.

balance(Pid) ->
    Pid ! {self(), {get_balance}},
    receive
        {ok, Balance} -> Balance;
        error -> error
    end.

charge(_Pid, _Amount) -> undefined.

-spec close(pid()) -> number().

close(Pid) ->
    Pid ! {self(), {close}},
    receive
        {ok, Amount} -> Amount;
        _unknown -> io:format("unknown message received", [])
    end,
    0.

start_bank_account() ->
    bank_account_process(#bank_account{}).

create() -> spawn(fun start_bank_account/0).

-spec deposit(pid(), number()) -> void.

deposit(Pid, Amount) ->
    Pid ! {self(), {deposit, Amount}}.

withdraw(_Pid, _Amount) -> undefined.
