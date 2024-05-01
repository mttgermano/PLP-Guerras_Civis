:- initialization main, halt.
:- include('Controllers/Menu.pl').

main :-
    writeln("[Main is working]"),
    menu_template("Start", StartMenu),
    menu_main(StartMenu).
