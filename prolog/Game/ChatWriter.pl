:- include('./../Controllers/Menu.pl').



menu_template("WriteChat",
    [
    "┌───────────────────────────── Guerras Civis ──────────────────────────────┐",
    "│Write your mensage                                                        │",
    "│                                                                          │",
    "└──────────────────────────────────────────────────────────────────────────┘"]).



write_chat(MenuTemplate) :- 
    cl,
    print_menu(MenuTemplate),
    read_line_to_string(user_input,Input),
    writeOut(Input),
    menu_template("Start", Menu),
    menu_main(Menu).












