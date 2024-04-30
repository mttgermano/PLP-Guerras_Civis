:- dynamic player/4.

% Test 
% player("PEDRO", room123, true).
% player("Djan", room123, true). 
% player("Matheus", room123, false).

% Player Actions ------------------------------------------------
add_player(Name, Password, Status) :-
    \+ player(Name, _, _, _),
    assertz(player(Name, Password, _, Status)).

delete_player(Name) :-
    retract(player(Name, _, _, _)),
    write('Player '), write(Name), write(' has been deleted.'), nl.

change_player_room(PlayerName, CurrentRoom, NewRoom) :-
    retract(player(PlayerName, ID, CurrentRoom, Status)),
    assertz(player(PlayerName, ID, NewRoom, Status)).

player_login(Name, Password) :-
    player(Name, Password, _, _).

% Player Utils --------------------------------------------------
get_player_room(Name, Room) :-
    player(Name, _, Room, _).

player_exists(Name) :-
    player(Name, _, _, _).

is_players_in_room(Room, Players) :-
    findall(X, player(X, _, Room, true), Players).

is_bots_in_room(Room, Bots) :-
    findall(X, player(X, _, Room, false), Bots).
