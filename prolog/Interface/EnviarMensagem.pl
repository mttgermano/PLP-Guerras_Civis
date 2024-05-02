send_message(Message) :-
    % Suponha que você tenha um contexto de usuário ou um nome de usuário padrão
    current_user(Username),
    % Suponha uma função que lide com o envio de mensagens
    broadcast_message(Username, Message),
    writeln("Mensagem enviada com sucesso!").
