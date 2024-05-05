:- dynamic room/6.
:- include('UserGameData.pl').

% Test
%room(room123, "PEDRO", false, "A", ['Djan Djan', 'Teste'], 'action', 0).

% Room Actions --------------------------------------------------
add_room(Name, Master, CursedWord) :-
    \+ room(Name, _, _, _, _, _),
    assertz(room(Name, Master, true, CursedWord, [], "A", 0)).

delete_room(Name) :-
    retract(room(Name, _, _, _, _, _, _)).

add_message_to_room(RoomName, Message) :-
    retract(room(RoomName, Master, Up, ForbiddenWord, Messages, State, Nround)),
    append(Messages, [Message], NewMessages),
    assertz(room(RoomName, Master, Up, ForbiddenWord, NewMessages, State, Nround)).

room_login(Rname, Pname) :-
    room(Rname, _, _, _, _, _),
    change_player_room(Pname,Rname).


% Room Utils ----------------------------------------------------
is_room_up(Name, Up) :-
    room(Name, _, Up, _, _, _).

get_room_state(Name, State, Nround) :-
    room(Name, _, _, _, _, State, Nround).

room_exists(Name) :-
    room(Name, _, _, _, _, _, _).

room_has_forbidden_word(_, false).
room_has_forbidden_word(RoomName, HasForbiddenWord) :-
    room(RoomName, _, _, ForbiddenWord, _, _, _),
    ForbiddenWord \= "",
    HasForbiddenWord = true.

get_room_master(RoomName, Master) :-
    room(RoomName, Master, _, _, _, _, _).

get_room_messages(Name, Messages) :-
    room(Name, _, _, _, Messages, _, _).

set_room_forbidden_word(RoomName, ForbiddenWord) :-
    retract(room(RoomName, Master, Up, _, Messages, State, Nround)),
    assertz(room(RoomName, Master, Up, ForbiddenWord, Messages, State, Nround)).

get_room_forbidden_word(Name, ForbiddenWord) :-
    room(Name, _, _, ForbiddenWord, _, _, _).

get_alive_players_in_room(Room, AlivePlayers) :-
    get_all_in_room(Room, Players),
    get_alive_players(Players, AlivePlayers).

is_role_alive_room(Room, Role) :-
    get_all_in_room(Room, Players),
    is_role_alive(Role, Players).

is_allowed_room(Agent, Name, State) :-
    get_room_state(Name, State, Nround),
    is_allowed(Agent).
