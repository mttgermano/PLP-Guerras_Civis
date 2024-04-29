:- initialization main, halt.

:- include('Controllers/Menu.pl').
:- include('Login/Player.pl').
:- include('Database/Players.pl').
:- include('Database/PlayersKnowledge.pl').
:- include('Database/UserGameData.pl').
% :- include('Game/BotLogic.pl').
% :- include('Game/RoleFunctions.pl').

main :-
    writeln("[Main is working]"),
    menu_template(Start, StartMenu),
    menu_main(StartMenu).
