:- dynamic know/3.

% Test
know("Pedro", "Djan").
know("Pedro", "Matheus").
know("Djan", "Matheus"). 

% Player Knowledge Actions --------------------------------------
add_knowledge(Name, NamePlayer) :-
    \+ know(Name, NamePlayer),
    assertz(know(Name, NamePlayer)).

remove_room_knowledge(Player) :-
    retractall(know(Player, _)),
    retractall(know(_, Player)).

knows(Name, NamePlayer) :-
    know(Name, NamePlayer).


% Player Knowledge Utils ----------------------------------------
get_knowledge(Person, KnowledgeList) :-
    findall((NamePlayer), know(Person, NamePlayer, _), KnowledgeList).
