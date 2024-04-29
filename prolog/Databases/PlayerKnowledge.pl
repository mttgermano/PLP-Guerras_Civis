:- dynamic knows/3.
knows("PEDRO", "Djan", room123).
knows("Djan", "Matheus", room123). 

add_knowledge(Name, NamePlayer, Room) :-
    \+ knows(Name, NamePlayer, _),
    \+ knows(NamePlayer, Name, _),
    assertz(knows(Name, NamePlayer, Room)).

remove_room_knowledge(Room) :-
    retractall(knows(_, _, Room)).

get_knowledge(Person, KnowledgeList) :-
    findall((NamePlayer), knows(Person, NamePlayer, _), KnowledgeList).
