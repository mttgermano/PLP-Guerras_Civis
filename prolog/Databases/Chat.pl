:- module(game_cli, [start_game_cli/0]).

% Importe módulos necessários aqui

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
