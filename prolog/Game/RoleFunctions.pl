kill(Agent, ActionReceiver) :-
    is_allowed(Agent, "action"),
    kill_vote(ActionReceiver),
%    format("> [~w] User - Kill Vote for [~w]", [Agent, ActionReceiver]),
    get_role(ActionReceiver, Role),
    (   Role =:= 1 -> fbiIsWatching(ActionReceiver, Agent)
    ;   true
    ).

apprentice(Agent, ActionReceiver) :-
    is_allowed(Agent, 'action'),
    get_player_room(Agent, Room),
    \+ is_role_alive_room(Room, 1),
    kill(Agent, ActionReceiver),
    get_role(ActionReceiver, Role),
    (   Role =:= 1 -> fbiIsWatching(ActionReceiver, Agent)
    ;   true
    ).

police(Agent, ActionReceiver) :-
    is_allowed(Agent, 'action'),
    get_player_room(Agent, Room),
    \+ is_role_alive_room(Room, 8),
    kill(Agent, ActionReceiver).

save(Agent, ActionReceiver) :-
    is_allowed(Agent, 'action'),
    get_role(ActionReceiver, Role),
    save_vote(ActionReceiver),
    (   Role =:= 1 -> fbiIsWatching(ActionReceiver, Agent)
    ;   true
    ).

search(Agent, ActionReceiver) :-
    is_allowed(Agent, 'action'),
    get_role(ActionReceiver, Role),
    (Role =:= 1 -> fbiIsWatching(ActionReceiver, Agent) ; true).

silence(Agent, ActionReceiver) :-
    isAllowed(Agent, 'action'),
    silence(ActionReceiver),
    get_role(ActionReceiver, Role),
    get_player_room(Agent, RName),
    add_message_to_room(RName, SilenceMessage),
    (Role =:= 1 -> fbiIsWatching(ActionReceiver, Agent) ; true).

paralise(Agent, ActionReceiver) :-
    isAllowed(Agent, 'action'),
    paralise(ActionReceiver),
    get_role(ActionReceiver, Role),
    get_player_room(Agent, RName),
    add_message_to_room(RName, SilenceMessage),
    (Role =:= 1 -> add_knowledge(ActionReceiver, Agent) ; true).

setCursedWord(Agent, CursedWord) :-
    isAllowed(Agent, 'action'),
    get_player_room(Agent, RName),
    \+ hasCursedWord(RName),
    set_room_forbidden_word(RName, CursedWord),
    format("> [~w] Already has cursed word~n", [RName]).

revenge(Agent, ActionReceiver) :-
    isAllowed(Agent, "action"),
    kill(Agent, ActionReceiver).

reveal(Agent, ActionReceiver) :-
    isAllowed(Agent, "action"),
    add_knowledge(Agent, ActionReceiver).

apply_reveal(_, [], _).
apply_reveal([Player|Rest], ActionReceiver) :-
    (Player =:= ActionReceiver -> true ; add_knowledge(Player, ActionReceiver)),
    apply_reveal(Rest, ActionReceiver).

paparazzi(Agent, ActionReceiver) :-
    isAllowed(Agent, "action"),
    get_player_room(Agent, Room),
    get_alive_players_in_room(Room, Players),
    apply_reveal(Players, ActionReceiver).