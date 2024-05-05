:- include('./../Databases/Rooms.pl').
:- include('./../Databases/Players.pl').

kill(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    is_allowed_room(Agent, Room, 'action'),
    write(ActionReceiver),
    write(' foi morto.'), nl,
    kill_vote(ActionReceiver),
    get_role(ActionReceiver, Role),
    (   Role =:= 8 -> fbiIsWatching(ActionReceiver, Agent)
    ;   true
    ).

apprentice(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    is_allowed_room(Agent, Room, 'action'),
    get_player_room(Agent, Room),
    \+ is_role_alive_room(Room, 1),
    kill(Agent, ActionReceiver),
    get_role(ActionReceiver, Role),
    (   Role =:= 8 -> fbiIsWatching(ActionReceiver, Agent)
    ;   true
    ).

police(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    is_allowed_room(Agent, Room, 'action'),
    get_player_room(Agent, Room),
    \+ is_role_alive_room(Room, 8),
    kill(Agent, ActionReceiver).

save(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    is_allowed_room(Agent, Room, 'action'),
    get_role(ActionReceiver, Role),
    save_vote(ActionReceiver),
    (   Role =:= 8 -> fbiIsWatching(ActionReceiver, Agent)
    ;   true
    ).

search(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    is_allowed_room(Agent, Room, 'action'),
    get_role(ActionReceiver, Role),
    (Role =:= 8 -> fbiIsWatching(ActionReceiver, Agent) ; true).

silence(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    is_allowed_room(Agent, Room, 'action'),
    silence(ActionReceiver),
    get_role(ActionReceiver, Role),
    get_player_room(Agent, RName),
    add_message_to_room(RName, 'Jogador silenciado'),
    (Role =:= 8 -> fbiIsWatching(ActionReceiver, Agent) ; true).

paralise(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    is_allowed_room(Agent, Room, 'action'),
    paralise(ActionReceiver),
    get_role(ActionReceiver, Role),
    get_player_room(Agent, RName),
    add_message_to_room(RName, 'Jogador silenciado'),
    (Role =:= 8 -> add_knowledge(ActionReceiver, Agent) ; true).

setCursedWord(Agent, CursedWord) :-
    get_player_room(Agent, Room),
    is_allowed_room(Agent, Room, 'action'),
    get_player_room(Agent, RName),
    \+ hasCursedWord(RName),
    set_room_forbidden_word(RName, CursedWord),
    format("> [~w] Already has cursed word~n", [RName]).

revenge(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    is_allowed_room(Agent, Room, 'action'),
    kill(Agent, ActionReceiver).

reveal(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    is_allowed_room(Agent, Room, 'action'),
    add_knowledge(Agent, ActionReceiver).

apply_reveal(_, [], _).
apply_reveal([Player|Rest], ActionReceiver) :-
    (Player =:= ActionReceiver -> true ; add_knowledge(Player, ActionReceiver)),
    apply_reveal(Rest, ActionReceiver).

paparazzi(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    is_allowed_room(Agent, Room, 'action'),
    get_player_room(Agent, Room),
    get_alive_players_in_room(Room, Players),
    apply_reveal(Players, ActionReceiver).