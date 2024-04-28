:- dynamic room/6.
room(room123, "PEDRO", false, "MATAR", [_], "VOTE").

add_room(Name, Master, CursedWord) :-
    \+ room(Name, _, _, _, _, _),
    assertz(room(Name, Master, true, CursedWord, [], "VOTE")).

delete_room(Name) :-
    retract(room(Name, _, _, _, _, _)).

is_room_up(Name, Up) :-
    room(Name, _, Up, _, _, _).

room_exists(Name) :-
    room(Name, _, _, _, _, _).

room_has_forbidden_word(RoomName, HasForbiddenWord) :-
    room(RoomName, _, _, ForbiddenWord, _, _),
    ForbiddenWord \= "",
    HasForbiddenWord = true.

room_has_forbidden_word(_, false).

room_master(RoomName, Master) :-
    room(RoomName, Master, _, _, _, _).

get_room_messages(Name, Messages) :-
    room(Name, _, _, _, Messages, _).