-module(scrabble_score).

-export([score/1]).

points(L) ->
    case string:str("aeioulnrst", L) > 0 of
        false -> 0;
        true -> 1
    end.

score(_Word) -> undefined.
