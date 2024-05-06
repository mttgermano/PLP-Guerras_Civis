:- dynamic know/2.

:- discontiguous know/2.
:- discontiguous knows/2.
:- discontiguous add_knowledge/2.
:- discontiguous remove_room_knowledge/1.
:- discontiguous get_knowledge/2.

% Test
% know("Pedro", "Djan").
% know("Pedro", "Djan3").
% know("Pedro", "Matheus").
% know("Djan", "Matheus"). 

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