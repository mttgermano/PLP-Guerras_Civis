select_the_index([], [], _, []).
select_the_index([X|Xs], [true|Flags], true, [X|Result]) :-
    select_the_index(Xs, Flags, true, Result).
select_the_index([_|Xs], [false|Flags], true, Result) :-
    select_the_index(Xs, Flags, true, Result).
select_the_index([X|Xs], [false|Flags], false, [X|Result]) :-
    select_the_index(Xs, Flags, false, Result).
select_the_index([_|Xs], [true|Flags], false, Result) :-
    select_the_index(Xs, Flags, false, Result).

