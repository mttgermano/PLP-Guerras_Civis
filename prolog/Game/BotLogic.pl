:- include('./../Databases/Rooms.pl').
:- use_module(library(uuid)).

botActionChoice(PlayerName, RName) :-
    get_alive_players_in_room(RName, Players),
    length(Players, Length),
    random(0, Length, Posicao),
    nth0(Posicao, Players, PlayerName).

createBots(0, RName)        :- format('> All Bots created in [~w]', [RName]).
createBots(Quant, RName)    :-
    uuid(UUID),
    atomic_list_concat(['bot-', UUID], BotName),
    add_player(BotName, "", true),
    add_user_game_data(BotName),
    NewQuant is Quant - 1,
    createBots(NewQuant, RName).

splitBySpaces([], []).
splitBySpaces([String|Rest], Result) :-
    splitBySpaces(Rest, RestResult),
    split_string(String, " ", "", StringList),
    append(StringList, RestResult, Result).

botBrain(RName, BotName) :-
    is_player_alive(BotName, BotAlive),
    ( BotAlive ->
        (  
            get_alive_players_in_room(RName, PlayersNames),
            get_room_messages(RName, Messages),
            splitBySpaces(Messages, AllWords),
            countReferencesForAll(AllWords, PlayersNames, References),
            get_knowledge(BotName, Players),
            compareIsGoodList(BotName, Players, PlayersNames, Comp),
            listSom(Comp, References, Results),
            index_of_max(Results, Ind, Max),
            nth0(Ind, PlayersNames, PlayerToIncrement),
            vote(PlayerToIncrement),
            get_role(BotName, BotRole),
            ( BotRole =:= 11 -> (
                    vote(PlayerToIncrement),
                    vote(PlayerToIncrement)
                ); true )
        ) ; true ).

compareIsGood(BotName, Players, PlayerName, Score) :-
    is_good(BotName, BotIsGood),
    is_good(PlayerName, PlayerIsGood),
    ((BotIsGood \== PlayerIsGood), member(PlayerName, Players)
    -> Score = 1000000
    ;(BotIsGood == PlayerIsGood, member(PlayerName, Players))
        -> Score = -100000
        ;  Score = 0).

compareIsGoodList(_, _, [], []).
compareIsGoodList(BotName, KnowList, [Player|RestPlayers], [Result|RestResults]) :-
    compareIsGood(BotName, KnowList, Player, Result),
    compareIsGoodList(BotName, KnowList, RestPlayers, RestResults).


index_of_max(List, Index, Max) :-
    max_list(List, Max),
    nth0(Index, List, Max).


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

%--------------------------------

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

:- dynamic(bot/1).

deleteBot(BUUID) :-
    retract(bot(BUUID)),
    format('Bot ~w deletado.~n', [BUUID]).

deleteRoomBots(RName) :-
    getRoomBots(RName, Bots),
    maplist(deleteBot, Bots),
    format("[~w] Room - Bots deletados.~n", [RName]).
