:- include('BotLogic.pl').

player_action(PlayerName, Action, 'action') :-
    get_role(PlayerName, Role), (
        Role =:= 1  -> kill(PlayerName, Action);
        Role =:= 2  -> apprentice(PlayerName, Action);
        Role =:= 3  -> reveal(PlayerName, Action);
        Role =:= 4  -> paralyze(PlayerName, Action);
        Role =:= 5  -> silence(PlayerName, Action);
        Role =:= 6  -> setCursedWord(PlayerName, Action);
        Role =:= 7  -> search(PlayerName, Action);
        Role =:= 8  -> kill(PlayerName, Action);
        Role =:= 9  -> police(PlayerName, Action);
        Role =:= 10 -> save(PlayerName, Action);
        Role =:= 11 -> write('Aldeao');
        Role =:= 12 -> revenge(PlayerName, Action);
        true
    ).

player_action(PlayerName, Action, 'vote') :-
    vote(PlayerName, Action).

start_game(Rname) :-
    Nbots is 11,
    write(Rname), nl,
    write(Nbots), nl,
    createBots(Nbots, RName),
    write('AQUI'), nl,
    assign_roles(Rname).