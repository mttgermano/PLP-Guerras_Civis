:- include('RoleFunctions.pl').

player_action(PlayerName, Action) :-
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
        Role =:= 11 -> true;
        Role =:= 12 -> revenge(PlayerName, Action);
    ).

start_game(Rname, Cpname) :-
    room(Rname, RoomMaster, IsAlive, CursedWord, Messages, Rstate),
    get_players_in_room(Cpname, Nplayers),
    length(Nplayers, Length),
    Nbots is 12 - Length,
    createBots(Nbots, RName),
    get_user_game_data(Cpname, Players, Alive, RoleList),
    maplist(start_knowledge, Players, RoleList).
