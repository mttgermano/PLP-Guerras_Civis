:- dynamic know/2.

% Test
know("Pedro", "Djan").
know("Pedro", "Djan3").
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
    findall((NamePlayer), know(Person, NamePlayer), KnowledgeList).