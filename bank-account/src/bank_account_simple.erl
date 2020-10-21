-module(bank_account_simple).

-export([balance/1,
         charge/2,
         close/1,
         create/0,
         deposit/2,
         withdraw/2]).

-record(bank_account, {balance = 0 :: number()}).

% ---------- API ----------------

balance(Pid) -> call_process(Pid, balance).

close(Pid) -> call_process(Pid, close).

create() -> spawn(fun start_bank_process/0).

deposit(_Pid, Amount) when Amount < 0 -> 0;
deposit(Pid, Amount) ->
    call_process(Pid, {deposit, Amount}).

withdraw(_Pid, Amount) when Amount < 0 -> 0;
withdraw(Pid, Amount) ->
    call_process(Pid, {withdraw, Amount}).

charge(_Pid, Amount) when Amount < 0 -> 0;
charge(Pid, Amount) ->
    call_process(Pid, {charge, Amount}).

% ---- bank account operations ----

update_balance(Account, Amount) ->
    NewBalance = Account#bank_account.balance + Amount,
    Account#bank_account{balance = NewBalance}.

deposit_amount(BankAccount, Amount) ->
    {update_balance(BankAccount, Amount), Amount}.

withdraw_amount(Account, Amount) ->
    Withdraw = min(Account#bank_account.balance, Amount),
    {update_balance(Account, -Withdraw), Withdraw}.

charge_amount(Account, Amount)
    when Amount > Account#bank_account.balance ->
    {Account, 0};
charge_amount(Account, Amount) ->
    {update_balance(Account, -Amount), Amount}.

get_balance(Account) ->
    {Account, Account#bank_account.balance}.

open_account() -> #bank_account{}.

% ---- process ----

handle_message(Message, State) ->
    case Message of
        {deposit, Amount} -> deposit_amount(State, Amount);
        {withdraw, Amount} -> withdraw_amount(State, Amount);
        {charge, Amount} -> charge_amount(State, Amount);
        balance -> get_balance(State)
    end.

bank_account_process(State) ->
    receive
        {From, close} ->
            From ! State#bank_account.balance,
            ok;
        {From, Message} ->
            {NextState, Result} = handle_message(Message, State),
            From ! Result,
            bank_account_process(NextState)
    end.

call_process(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        Msg -> Msg
        after 500 ->
                  case is_process_alive(Pid) of
                      true -> {error, timeout};
                      false -> {error, account_closed}
                  end
    end.

start_bank_process() ->
    bank_account_process(open_account()).
