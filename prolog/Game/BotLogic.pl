botActionChoice(BotName, RName, PlayerName) :-
    players_in_room(RName, Players),
    length(Players, Length),
    random(0, Length, Posicao),
    nth0(Posicao, Players, PlayerName),
    is_player_alive(PlayerName, Alive),
    Alive == true,
    BotName \= PlayerName.

createBots(Quant, RName) :-
    random_uuid(UUID),
    atomic_list_concat(['bot-', UUID], BotName),
    add_player(BotName, "", RName, true),
    add_user_game_data(BotName),
    NewQuant is Quant - 1,
    createBots(NewQuant, RName).

createBots(0, RName) :-
    format('> All Bots created in [~w]', [RName]).

splitBySpaces([], []).

splitBySpaces([String|Rest], Result) :-
    splitBySpaces(Rest, RestResult),
    split_string(String, " ", "", StringList),
    append(StringList, RestResult, Result).

botBrain(RName, BotName) :-
    is_player_alive(BotName, BotAlive),
    (
        BotAlive = false
    ;
        BotAlive = true,
        get_alive_players(RName, PlayersNames),
        get_room_messages(PlayersNames, Messages),
        splitBySpaces(Messages, AllWords),
        countReferencesForAll(AllWords, PlayersNames, References),
        getPlayerKnowledgeList(BotName, Players),
        compareIsGoodList(BotName, PlayersNames, Players, Comparison),
        call(Comparison, Comp),
        listSom(Comp, References, Results),
        biggestVote(Results, Ind),
        nth0(Ind, PlayersNames, PlayerToIncrement),
        incrementVote(BName, PlayerToIncrement),
        getRole(BotName, BotRole),
        (
            BotRole = 11,
            incrementVote(BName, PlayerToIncrement),
            incrementVote(BName, PlayerToIncrement)
        ;
            true
        )
    ).

compareIsGoodIsAlive(BotName, Players, PlayerName, Score) :-
    getIsGood(BotName, BotIsGood),
    getIsGood(PlayerName, PlayerIsGood),
    isPlayerAlive(PlayerName, PlayerAlive),
    (   (BotIsGood \== PlayerIsGood), member(PlayerName, Players), PlayerAlive
    ->  Score = 1000000
    ;   (BotIsGood == PlayerIsGood, member(PlayerName, Players)); \+ PlayerAlive
    ->  Score = -100000
    ;   Score = 0
    ).

compareIsGoodList(BotName, Players, Result) :-
    maplist(compareIsGoodIsAlive(BotName, Players), Result).


biggest_vote_index(Xs, Index) :-
    biggest_vote_index(Xs, 0, -1, Index).

biggest_vote_index([], _, MaxIndex, MaxIndex).
biggest_vote_index([X|Xs], CurrIndex, MaxIndexSoFar, MaxIndex) :-
    NewIndex is CurrIndex + 1,
    (   X > MaxIndexSoFar
    ->  biggest_vote_index(Xs, NewIndex, X, MaxIndex)
    ;   biggest_vote_index(Xs, NewIndex, MaxIndexSoFar, MaxIndex)
    ).

countReferencesForAll(_, [], []).
countReferencesForAll(Words, [X|Xs], [Count|Counts]) :-
    nameCountReferences(X, Words, Count),
    countReferencesForAll(Words, Xs, Counts).

listSom([], [], []).
listSom([], Ys, Ys).
listSom(Xs, [], Xs).
listSom([X|Xs], [Y|Ys], [Z|Zs]) :-
    Z is X + Y,
    listSom(Xs, Ys, Zs).
