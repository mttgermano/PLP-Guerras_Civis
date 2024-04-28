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

alive_players_in_room(Room, alive, Players) :-
    findall(X, player(X, _, Room, true), Players).

bots_in_room(Room, Bots) :-
    findall(X, player(X, _, Room, false), Bots).

player_room(Name, Room) :-
    player(Name, _, Room, _).

player_alive(Name, Alive) :-
    player(Name, _, _, Alive).