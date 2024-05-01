kill(Agent, ActionReceiver) :-
    is_allowed(Agent, "action"),
    kill_vote(ActionReceiver),
    format("> [~w] User - Kill Vote for [~w]", [Agent, ActionReceiver]),
    get_role(ActionReceiver, Role),
    (   Role =:= 1 -> fbiIsWatching(ActionReceiver, Agent)
    ;   true
    ).

apprentice(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    (   is_allowed(Agent, 'action'),
        \+ IsAssassinAlive,
        is_role_alive_room(Room, 1)
    ->  kill(Agent, ActionReceiver),
        get_role(ActionReceiver, Role),
        (   Role =:= 1
        ->  fbiIsWatching(ActionReceiver, Agent)
        ;   true
        )
    ;   errPermissionMessage(Agent)
    ).

police(Agent, ActionReceiver) :-
    get_player_room(Agent, Room),
    (   is_allowed(Agent, 'action'),
        is_role_alive_room(Room, 8)
    ->  kill(Agent, ActionReceiver)
    ;   errPermissionMessage(Agent)
    ).

save(Agent, ActionReceiver) :-
    (   is_allowed(Agent, 'action') ->
            format("> [~w] User - Saved [~w]", [Agent, ActionReceiver]),
            get_role(ActionReceiver, Role),
            (   Role =:= 1 ->
                    fbiIsWatching(ActionReceiver, Agent)
                ;   true
            )
        ;   errPermissionMessage(Agent)
    ).

search(Agent, ActionReceiver) :-
    is_allowed(Agent, 'action'),
    (Role =:= 1 -> fbiIsWatching(ActionReceiver, Agent) ; true),
    get_role(ActionReceiver, Role),
    format("> [~s] User - Searched [~s]", [Agent, ActionReceiver]),
    (Role =:= 1 -> fbiIsWatching(ActionReceiver, Agent) ; true).
    
search(Agent, _) :-
    errPermissionMessage(Agent).

silence(Agent, ActionReceiver) :-
    isAllowed(Agent, 'action'),
    silence(ActionReceiver),
    get_role(ActionReceiver, Role),
    getPlayerRoomName(AgentUuid, RName),
    format(atom(SilenceMessage), 'User ~w foi silenciado.', [ActionReceiver]),
    admSendMessage(RName, SilenceMessage),
    (   Role =:= 1
    ->  fbiIsWatching(ActionReceiver, Agent)
    ;   true
    ).

paralize(Agent, ActionReceiver) :-
    isAllowed(Agent, 'action'),
    format(string(Msg), "> [~w] User - Paralized [~w]", [Agent, ActionReceiver]),
    writeln(Msg),
    get_role(ActionReceiver, Role),
    getPlayerRoomName(AgentUuid, RoomName),
    format(atom(Message), "User ~w foi paralisado.", [ActionReceiver]),
    admSendMessage(RoomName, Message),
    (Role =:= 1 ->
        fbiIsWatching(ActionReceiver, Agent)
    ;
        true
    ).
paralize(Agent, _) :-
    errPermissionMessage(Agent).


setCursedWord(Agent, CursedWord) :-
    isAllowed(Agent, 'action'),
    getUUIDFromPlayerName(Agent, AgentUuid),
    getPlayerRoomName(AgentUuid, RName),
    hasCursedWord(RName),
    !,
    format("> [~w] Already has cursed word~n", [RName]).
    
setCursedWord(Agent, CursedWord) :-
    isAllowed(Agent, 'action'),
    getPlayerRoomName(AgentUuid, RName),
    assert(hasCursedWord(RName)),
    format("> [~w] User - setted Cursed Word for ~w~n", [Agent, RName]).
    
setCursedWord(Agent, _) :-
    errPermissionMessage(Agent).


% Predicate to check revenge
revenge(Agent, ActionReceiver) :-
    isAllowed(Agent, "action"),
    kill(Agent, ActionReceiver).

revenge(Agent, _) :- 
    errPermissionMessage(Agent).

