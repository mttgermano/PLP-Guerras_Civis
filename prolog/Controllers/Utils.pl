% Utils para impressão de interfaces para Menu.pl e Game_Menu.pl.
print_menu([]).
print_menu([X|Xs]) :-
    writeln(X),  
    print_menu(Xs).

cl :- (current_prolog_flag(windows, true) -> shell('cls'); shell('clear')).
%cl :- writeln(5).    % use it for debug
