-module(bank_account_core).

-export([charge_amount/2,
         close_account/1,
         deposit_amount/2,
         get_balance/1,
         open_account/0,
         withdraw_amount/2]).

-type bank_account_status() :: active | closed.

-type bank_account_error() :: account_closed |
                              not_enough_money.

-type result(R) :: {ok, Result :: R} |
                   {error, bank_account_error()}.

-record(bank_account,
        {balance = 0 :: number(),
         status = active :: bank_account_status()}).

-type bank_account() :: #bank_account{}.

-spec update_balance(bank_account(),
                     number()) -> bank_account().

update_balance(Account = #bank_account{balance = B},
               Amount) ->
    Account#bank_account{balance = B + Amount}.

-spec deposit_amount(bank_account(),
                     number()) -> result({Account :: bank_account(),
                                          AmountDeposited :: number()}).

deposit_amount(#bank_account{status = closed}, _) ->
    {error, account_closed};
deposit_amount(BankAccount, Amount) ->
    UpdatedAccount = update_balance(BankAccount, Amount),
    {ok, {UpdatedAccount, Amount}}.

-spec withdraw_amount(bank_account(),
                      number()) -> result({Account :: bank_account(),
                                           AmountWithdrawn :: number()}).

withdraw_amount(#bank_account{status = closed}, _) ->
    {error, account_closed};
withdraw_amount(Account = #bank_account{balance = B},
                Amount) ->
    Withdraw = min(B, Amount),
    {ok, {update_balance(Account, -Withdraw), Withdraw}}.

-spec charge_amount(bank_account(),
                    number()) -> result({Account :: bank_account(),
                                         AmountCharged :: number()}).

charge_amount(#bank_account{status = closed}, _) ->
    {error, account_closed};
charge_amount(#bank_account{balance = B}, Amount)
    when Amount > B ->
    {error, not_enough_money};
charge_amount(BankAccount, Amount) ->
    {ok, {update_balance(BankAccount, -Amount), Amount}}.

-spec get_balance(bank_account()) -> result(number()).

get_balance(#bank_account{status = closed}) ->
    {error, account_closed};
get_balance(BankAccount) ->
    {ok, BankAccount#bank_account.balance}.

-spec
     close_account(bank_account()) -> result({Account ::
                                                  bank_account(),
                                              Amount :: number()}).

close_account(BankAccount) ->
    {ok, {BankAccount#bank_account{status = closed}, 0}}.

-spec open_account() -> bank_account().

open_account() -> #bank_account{}.
