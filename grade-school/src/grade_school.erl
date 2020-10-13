-module(grade_school).

-export([add/3, get/1, get/2, new/0]).

-record(student, {name, grade}).

-record(school, {students = [] :: [student]}).

sort_student_fn(Left, Right) ->
    if Left#student.grade == Right#student.grade ->
           Left#student.name =< Right#student.name;
       true -> Left#student.grade =< Right#student.grade
    end.

get_names(Students) ->
    lists:map(fun (S) -> S#student.name end, Students).

add(Name, Grade, School) ->
    Student = #student{name = Name, grade = Grade},
    School#school{students =
                      [Student | School#school.students]}.

get(Grade, School) ->
    Match_Grade = fun (S) -> S#student.grade == Grade end,
    Students = lists:filter(Match_Grade,
                            School#school.students),
    get_names(Students).

get(School) ->
    F = fun (L, R) -> sort_student_fn(L, R) end,
    Sorted = lists:sort(F, School#school.students),
    get_names(Sorted).

new() -> #school{}.
