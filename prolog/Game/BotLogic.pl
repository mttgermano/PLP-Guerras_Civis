:- include('RoleFunctions.pl').
:- use_module(library(uuid)).

botActionChoice(RName, Chosen) :-
    get_alive_players_in_room(RName, Players),
    length(Players, Length),
    random(0, Length, Posicao),
    nth0(Posicao, Players, Chosen).

createBots(0, _)        :- true.
createBots(Quant, RName)    :-
    atomic_list_concat(['bot-', Quant], BotName),
    add_player(BotName, "", true),
    change_player_room(BotName, RName),
    %add_user_game_data(BotName),
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
            write(BotName), nl,
            write(PlayerToIncrement), nl,
            get_role(BotName, BotRole),
            ( BotRole =:= 11 -> (
                    vote(PlayerToIncrement),
                    vote(PlayerToIncrement))
                    ; true )
        ) 
        ; true).

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

botAction(Bot, RName) :-
    get_role(Bot, BotRole),
    botActionChoice(RName, Player),
    randomWord(ChoiceWord),
    (
        BotRole =:= 1 -> kill(Bot, Player)
        ;   BotRole =:= 2 -> apprentice(Bot, Player)
        ;   BotRole =:= 3 -> reveal(Bot, Player)
        ;   BotRole =:= 4 -> paralyze(Bot, Player)
        ;   BotRole =:= 5 -> silence(Bot, Player)
        ;   BotRole =:= 6 -> setCursedWord(Bot, ChoiceWord)
        ;   BotRole =:= 7 -> search(Bot, Player)
        ;   BotRole =:= 8 -> kill(Bot, Player)
        ;   BotRole =:= 9 -> police(Bot, Player)
        ;   BotRole =:= 10 -> save(Bot, Player)
        ;   BotRole =:= 12 -> revenge(Bot, Player)
        ;   true
    ).
callBots([], _).
callBots([BotName|Rest], RName) :-
    botAction(BotName, RName),
    callBots(Rest, RName).


botsRound(RName) :-
    get_bots_in_room(RName, Bots),
    format("> [~w] Room - Comecou Bot Round ~n", [RName]),
    callBots(Bots, RName),
    format("> [~w] Room - Terminou Bot Round ~n", [RName]).

callBotsVote([], _).
callBotsVote([BotName|Rest], RName) :-
    botBrain(RName, BotName),
    callBotsVote(Rest, RName).

voteBotsRound(RName) :-
    get_bots_in_room(RName, Bots),
    format("> [~w] Room - Comecou Bot Vote ~n", [RName]),
    callBotsVote(Bots, RName),
    format("> [~w] Room - Terminou Bot Vote ~n", [RName]).

deleteBot(Name) :-
    delete_player(Name),
    format('Bot ~w deletado.~n', [Name]).


deleteRoomBots(RName) :-
    get_bots_in_room(RName, Bots),
    maplist(deleteBot, Bots),
    format("[~w] Room - Bots deletados.~n", [RName]).
