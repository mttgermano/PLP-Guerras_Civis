:- include('./../Databases/Rooms.pl').

kill(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    (   is_allowed_room(Agent, Room, 'action') ->
        write(ActionReceiver),
        write(' foi morto.'), nl,
        kill_vote(ActionReceiver),
        get_role(ActionReceiver, Role),
        (   Role =:= 8 ->reveal(Agent, ActionReceiver) ;   true
        )
    ;   true
    ).

apprentice(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    (   is_allowed_room(Agent, Room, 'action'),
        \+ is_role_alive_room(Room, 1) ->
        kill(Agent, ActionReceiver),
        get_role(ActionReceiver, Role),
        (   Role =:= 8 -> reveal(Agent, ActionReceiver)
        ;   true)
    ;   true
    ).

police(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    (   is_allowed_room(Agent, Room, 'action'),
        \+ is_role_alive_room(Room, 8) ->
        write(ActionReceiver),
        write(' foi morto.'), nl,
        kill(Agent, ActionReceiver)
    ;   true
    ).

save(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    (   is_allowed_room(Agent, Room, 'action') ->
        get_role(ActionReceiver, Role),
        write(ActionReceiver),
        write(' foi salvo.'), nl,
        save_vote(ActionReceiver),
        (   Role =:= 8 -> reveal(Agent, ActionReceiver)
        ;   true)
    ;   true
    ).

search(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    (   is_allowed_room(Agent, Room, 'action') ->
        get_role(ActionReceiver, Role),
        reveal(Agent, ActionReceiver),
        (   Role =:= 8 -> reveal(Agent, ActionReceiver)
        ;   true
        )
    ;   true
    ).

silence(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
     (   is_allowed_room(Agent, Room, 'action') ->
        silence_player(ActionReceiver),
        get_role(ActionReceiver, Role),
        get_player_room(Agent, RName),
        add_message_to_room(RName, 'Jogador silenciado'),
        write(ActionReceiver),
        write(' foi silenciado.'), nl,
        (   Role =:= 8 -> reveal(Agent, ActionReceiver)
        ;   true)
    ;   true).

paralyze(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    (   is_allowed_room(Agent, Room, 'action') ->
        paralise(ActionReceiver),
        get_role(ActionReceiver, Role),
        get_player_room(Agent, RName),
        write(ActionReceiver),
        write(' foi paralisado.'), nl,
        add_message_to_room(RName, 'Jogador silenciado'),
        (   Role =:= 8 -> add_knowledge(ActionReceiver, Agent)
        ;   true)
    ;   true).

setCursedWord(Agent, CursedWord) :-
    get_player_room(Agent, Room),
    (   is_allowed_room(Agent, Room, 'action'),
        \+ room_has_forbidden_word(Room) ->
        set_room_forbidden_word(Room, CursedWord),
        format("> [~w] Already has cursed word~n", [Room])
    ;   true).

revenge(Agent, ActionReceiver) :-
    is_player_alive(Agent, Alive),
    (   Alive ->
        true
    ;   kill(Agent, ActionReceiver)).

reveal(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    (   is_allowed_room(Agent, Room, 'action') ->
        write(ActionReceiver),
        write(' foi revelado.'), nl,
        add_knowledge(Agent, ActionReceiver)
    ;   true).

apply_reveal(_, [], _).
apply_reveal([Player|Rest], ActionReceiver) :-
    (Player =:= ActionReceiver -> true ; add_knowledge(Player, ActionReceiver)),
    apply_reveal(Rest, ActionReceiver).

paparazzi(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    (   is_allowed_room(Agent, Room, 'action') ->
        get_alive_players_in_room(Room, Players),
        apply_reveal(Players, ActionReceiver)
    ;   true).