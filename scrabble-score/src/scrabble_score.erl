-module(scrabble_score).

-export([score/1]).

-record(score_rule, {letters, score}).

get_score_rules() ->
    [#score_rule{letters = "AEIOULNRST", score = 1},
     #score_rule{letters = "DG", score = 2},
     #score_rule{letters = "BCMP", score = 3},
     #score_rule{letters = "FHVWY", score = 4},
     #score_rule{letters = "K", score = 5},
     #score_rule{letters = "JX", score = 8},
     #score_rule{letters = "QZ", score = 10}].

transform_word(Word) -> string:to_upper(Word).

match_rule(Rule, Letter) ->
    string:str(Rule#score_rule.letters, Letter) > 0.

find_letter_score([], _Letter) -> 0;
find_letter_score(Rules, Letter) ->
    [Head | Rest] = Rules,
    case match_rule(Head, Letter) of
        true -> Head#score_rule.score;
        false -> find_letter_score(Rest, Letter)
    end.

find_word_score(_Rules, [], Acc) -> Acc;
find_word_score(Rules, Word, Acc) ->
    [Letter | Rest] = Word,
    Total = find_letter_score(Rules, [Letter]) + Acc,
    find_word_score(Rules, Rest, Total).

score(Word) ->
    Rules = get_score_rules(),
    find_word_score(Rules, transform_word(Word), 0).
