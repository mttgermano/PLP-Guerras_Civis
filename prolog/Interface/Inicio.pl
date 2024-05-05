
:- module(game_cli, [start_game_cli/0]).

% Parte da tela inicial (Maria Clara Rodrigues %do Nascimento)
start_game_cli :-
    writeln("Bem-vindo ao Guerras Civis CLI!"),
    writeln("Escolha uma opção:"),
    writeln("1. Ação"),
    writeln("2. Mensagem"),
    writeln("3. Sair"),
    read(Choice),
    handle_choice(Choice).

handle_choice(1) :- 
    list_alive_players,
    writeln("Escolha um jogador para atacar:"),
    read(Target),
    execute_action(Target),
    start_game_cli.

handle_choice(2) :- 
    writeln("Digite sua mensagem:"),
    read(Message),
    send_message(Message),
    start_game_cli.

handle_choice(3) :-
    writeln("Saindo do jogo..."),
    !.

handle_choice(_) :-
    writeln("Opção inválida, tente novamente."),
    start_game_cli.

% Implementações de funções de ação, mensagem, listagem de jogadores, etc.
list_alive_players :-
    findall(Name, player(Name, _, true), AlivePlayers),
    list_players_with_numbers(AlivePlayers, 1).

list_players_with_numbers([], _).
list_players_with_numbers([H|T], Index) :-
    format("~w. ~w~n", [Index, H]),
    NextIndex is Index + 1,
    list_players_with_numbers(T, NextIndex).

send_message(Message) :-
    % Suponha que você tenha um contexto de usuário ou um nome de usuário padrão
    current_user(Username),
    % Suponha uma função que lide com o envio de mensagens
    broadcast_message(Username, Message),
    writeln("Mensagem enviada com sucesso!").

execute_action(Index) :-
    findall(Name, player(Name, _, true), AlivePlayers),
    nth1(Index, AlivePlayers, Target),  % Pegar o jogador baseado no índice
    % Suponha uma função attack_player que executa a ação
    attack_player(Target),
    writeln("Ação executada com sucesso!").
