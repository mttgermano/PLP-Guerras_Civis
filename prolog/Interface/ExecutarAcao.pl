execute_action(Index) :-
    findall(Name, player(Name, _, true), AlivePlayers),
    nth1(Index, AlivePlayers, Target),  % Pegar o jogador baseado no índice
    % Suponha uma função attack_player que executa a ação
    attack_player(Target),
    writeln("Ação executada com sucesso!").
