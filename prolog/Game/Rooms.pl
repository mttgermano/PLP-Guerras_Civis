:- dynamic room/6.
room(room123, "PEDRO", true, "MATAR", [_], "VOTE").

add_room(Name, Master, CursedWord) :-
    \+ room(Name, _, _, _, _, _),
    assertz(room(Name, Master, true, CursedWord, [], "VOTE")).

delete_room(Name) :-
    retract(room(Name, _, _, _, _, _)).