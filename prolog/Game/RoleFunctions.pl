
kill(Agent, ActionReceiver) :-
    isAllowed(Agent, 'action', Allowed),
    (   Allowed = true ->
        getUUIDFromPlayerName(ActionReceiver, ActionReceiverUUID),
        getDbConnection(Connection),
        sqlQuery(Query),
        format(atom(QueryAtom), 'UPDATE UserGameData SET kill_vote = kill_vote + 1 WHERE player_uuid = ~w', [ActionReceiverUUID]),
        queryDb(Connection, QueryAtom),
        format('>', []),
        format(' [~w] User - Kill Vote for [~w]~n', [Agent, ActionReceiver]),
        close(Connection),
        getRole(ActionReceiver, Role),
        (   Role = 1 ->
            fbiIsWatching(ActionReceiver, Agent)
        ;   true
        )
    ;   errPermissionMessage(Agent)
    ).