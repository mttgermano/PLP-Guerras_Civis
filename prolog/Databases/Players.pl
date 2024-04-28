:- dynamic player/4.
player("PEDRO", 12, room123, true).
player("Djan", 12345, room123, true). 
player("Matheus", 12345, room123, false).

add_player(Name, ID, Room, Status) :-
    \+ player(Name, _, _, _),
    assertz(player(Name, ID, Room, Status)).

player_room(Name, Room) :-
    player(Name, _, Room, _).

player_exists(Name) :-
    player(Name, _, _, _).

delete_player(Name) :-
    retract(player(Name, _, _, _)),
    write('Player '), write(Name), write(' has been deleted.'), nl.

players_in_room(Room, Players) :-
    findall(X, player(X, _, Room, true), Players).

bots_in_room(Room, Bots) :-
    findall(X, player(X, _, Room, false), Bots).

player_room(Name, Room) :-
    player(Name, _, Room, _).

change_player_room(PlayerName, CurrentRoom, NewRoom) :-
    retract(player(PlayerName, ID, CurrentRoom, Status)),
    assertz(player(PlayerName, ID, NewRoom, Status)).

login(Name, Password) :-
    player(Name, Password, _, _).