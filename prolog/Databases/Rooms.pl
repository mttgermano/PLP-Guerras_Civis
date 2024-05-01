:- dynamic room/6.
:- include('./../Databases/UserGameData.pl').
:- include('./../Databases/Players.pl').

% Test
% room(room123, "PEDRO", false, "MATAR", [], "VOTE").

% Room Actions --------------------------------------------------
add_room(Name, Master, CursedWord) :-
    \+ room(Name, _, _, _, _, _),
    assertz(room(Name, Master, true, CursedWord, [], "VOTE")).

delete_room(Name) :-
    retract(room(Name, _, _, _, _, _)).

add_message_to_room(RoomName, Message) :-
    retract(room(RoomName, Master, Up, ForbiddenWord, Messages, State)),
    append(Messages, [Message], NewMessages),
    assertz(room(RoomName, Master, Up, ForbiddenWord, NewMessages, State)).

% room_login(Rname, Pname) :-
%     % TODO
%     % atualizar player
%     room(Rname, _, _, _, _, _).


% Room Utils ----------------------------------------------------
is_room_up(Name, Up) :-
    room(Name, _, Up, _, _, _).

get_room_state(Name, State) :-
    room(Name, _, _, _, _, State).

room_exists(Name) :-
    room(Name, _, _, _, _, _).

room_has_forbidden_word(RoomName, HasForbiddenWord) :-
    room(RoomName, _, _, ForbiddenWord, _, _),
    ForbiddenWord \= "",
    HasForbiddenWord = true.

room_has_forbidden_word(_, false).

get_room_master(RoomName, Master) :-
    room(RoomName, Master, _, _, _, _).

get_room_messages(Name, Messages) :-
    room(Name, _, _, _, Messages, _).

get_room_forbidden_word(Name, ForbiddenWord) :-
    room(Name, _, _, ForbiddenWord, _, _).

get_alive_players_in_room(Room, AlivePlayers) :-
    get_all_in_room(Room, Players),
    get_alive_players(Players, AlivePlayers).