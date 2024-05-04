:- initialization main, halt.

:- include('Controllers/Menu.pl').
:- include('Controllers/Utils.pl').
:- include('Databases/Players.pl').
:- include('Databases/Rooms.pl').
:- include('Databases/PlayersKnowledge.pl').
:- include('Databases/UserGameData.pl').
% :- include('Game/BotLogic.pl').
% :- include('Game/RoleFunctions.pl').

main :-
    writeln("[Main is working]"),
    menu_template("Start", StartMenu),
    menu_main(StartMenu).
