:- dynamic knows/3.

% Test
% knows("PEDRO", "Djan", room123).
% knows("Djan", "Matheus", room123). 

% Player Knowledge Actions --------------------------------------
add_knowledge(Name, NamePlayer, Room) :-
    \+ knows(Name, NamePlayer, _),
    \+ knows(NamePlayer, Name, _),
    assertz(knows(Name, NamePlayer, Room)).

remove_room_knowledge(Room) :-
    retractall(knows(_, _, Room)).


% Player Knowledge Utils ----------------------------------------
get_knowledge(Person, KnowledgeList) :-
    findall((NamePlayer), knows(Person, NamePlayer, _), KnowledgeList).
