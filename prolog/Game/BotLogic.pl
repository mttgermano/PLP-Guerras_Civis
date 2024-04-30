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

nameCountReferences(_, [], 0).
nameCountReferences(Player, [Player|PlayerNames], Count) :-
    nameCountReferences(Player, PlayerNames, SubCount),
    Count is SubCount + 1.
nameCountReferences(Player, [_|PlayerNames], Count) :-
    nameCountReferences(Player, PlayerNames, Count).

possibleWords(["sinto", "acho", "teste", "livro", "água", "banana", "futebol", "computador", "amor", "tempo", "cidade", "felicidade"]).

randomWord(Word) :-
    possibleWords(Words),
    length(Words, Length),
    random(0, Length, Index),
    nth0(Index, Words, Word).

botAction(BotId, RName) :-
    getRole(BotId, BotRole),
    botActionChoice(BotId, RName, PlayerId),
    randomWord(ChoiceWord),
    getPlayerNameFromUUID(PlayerId, PlayerName),
    getPlayerNameFromUUID(BotId, BotName),
    isPlayerAlive(BotId, BotAlive),
    (
        BotAlive = false -> true
    ;
        (
            BotRole = 1 -> kill(BotName, PlayerName)
        ;   BotRole = 2 -> apprentice(BotName, PlayerName)
        ;   BotRole = 3 -> reveal(BotName, PlayerName)
        ;   BotRole = 4 -> paralize(BotName, PlayerName)
        ;   BotRole = 5 -> silence(BotName, PlayerName)
        ;   BotRole = 6 -> setCursedWord(BotName, ChoiceWord)
        ;   BotRole = 7 -> search(BotName, PlayerName)
        ;   BotRole = 8 -> kill(BotName, PlayerName)
        ;   BotRole = 9 -> police(BotName, PlayerName)
        ;   BotRole = 10 -> save(BotName, PlayerName)
        ;   BotRole = 12 -> revenge(BotName, PlayerName)
        ;   true -> format(".")
        )
    ).
callBots([], _).
callBots([BotId|Rest], RName) :-
    botAction(BotId, RName),
    callBots(Rest, RName).

botsRound(RName) :-
    getRoomBots(RName, Bots),
    format("> [~w] Room - Começou Bot Round", [RName]),
    callBots(Bots, RName),
    format("> [~w] Room - Terminou Bot Round", [RName]).

callBotsVote([], _).
callBotsVote([BotId|Rest], RName) :-
    botBrain(RName, BotId),
    callBotsVote(Rest, RName).

voteBotsRound(RName) :-
    getRoomBots(RName, Bots),
    format("> [~w] Room - Começou Bot Vote", [RName]),
    callBotsVote(Bots, RName),
    format("> [~w] Room - Terminou Bot Vote", [RName]).

deleteRoomBots(RName) :-
    getRoomBots(RName, Bots),
    maplist(deleteBot, Bots),
    format("> [~w] Room - bots deletados", [RName]).

deleteBot(BUUID) :-
    getDbConnection(Connection),
    SQLQuery = "DELETE FROM Player WHERE player_uuid = ?",
    execute(Connection, SQLQuery, [BUUID]),
    close(Connection).

getDbConnection(Connection) :- db_connection(Connection).
execute(Connection, SQLQuery, Values) :- db_execute(Connection, SQLQuery, Values).
close(Connection) :- db_close(Connection).