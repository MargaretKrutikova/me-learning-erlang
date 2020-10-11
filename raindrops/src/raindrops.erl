-module(raindrops).

-export([convert/1]).

isPling(Number) when Number rem 3 == 0 -> "Pling";
isPling(_) -> "".

isPlang(Number) when Number rem 5 == 0 -> "Plang";
isPlang(_) -> "".

isPlong(Number) when Number rem 7 == 0 -> "Plong";
isPlong(_) -> "".

raindrops(Number) ->
    isPling(Number) ++ isPlang(Number) ++ isPlong(Number).

convert(Number) ->
    case raindrops(Number) of
        "" -> integer_to_list(Number);
        Sound -> Sound
    end.
