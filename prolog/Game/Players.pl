
:- dynamic player/4.
player("PEDRO", 12, room123, true).
player("Djan", 12345, room123, true). 
player("Matheus", 12345, room123, false).

add_player(Name, ID, Room, Status) :-
    \+ player(Name, _, _, _), % Check if player with the same name doesn't already exist
    assertz(player(Name, ID, Room, Status)).