import System.Console.ANSI
import System.Console.Terminal.Size (size, width)

--aquivo com funcoes basicas para cli de um jogador

limpatela :: IO ()
limpatela = clearScreen >> setCursorPosition 0 0

escreveNoCentro :: String -> IO ()
escreveNoCentro palaVra = do
    larguraTerm <- maybe 80 width <$> size 
    let padding = max 0 $ (larguraTerm - length palaVra) `div` 2

    let esquerda = replicate padding '-'
    let direita = replicate (larguraTerm - length palaVra - padding) '-'
    
    putStrLn $ esquerda ++ palaVra ++ direita


linhaStatusPlayer :: String -> String -> String -> IO()
linhaStatusPlayer player role  status = do
	larguraTerm <- maybe 80 width <$> size
	let padding = max 0 $ larguraTerm
	let direita = replicate (larguraTerm - length player - length role - length status -9) ' '
	putStrLn $ "| " ++ player ++ "   " ++ status ++ "   " ++role ++ direita ++ "|"

linhaSeparacao :: IO ()
linhaSeparacao = do
    larguraTerm <- maybe 80 width <$> size
    let linha = replicate (larguraTerm-2) '_'
    putStrLn $ "|" ++ linha ++ "|"



--criar buffer de mensagens >:?
mensagemUsuario :: String -> IO ()
mensagemUsuario mensagem = do
	larguraTerm <- maybe 80 width <$> size
	let padding = max 0 $ larguraTerm
	let mensagemDef = "Mensagem : "
	let direita = replicate (larguraTerm - length mensagem - 3 - length mensagemDef) ' '
	putStrLn $ "| " ++ mensagemDef ++ mensagem ++ direita ++ "|"

	linhaSeparacao 

menuSelecao :: IO ()
menuSelecao = do
    larguraTerm <- maybe 80 width <$> size
    let padding = max 0 $ larguraTerm
        direita = replicate (larguraTerm - length "[1] realizar ação" - 3) ' '
    putStrLn $ "| [1] realizar ação" ++ direita ++ "|"
    let direita2 = replicate (larguraTerm - length "[2] enviar mensagem" - 3) ' '
    putStrLn $ "| [2] enviar mensagem" ++ direita2 ++ "|";
    opcao <- getLine ;
    linhaSeparacao ;

main :: IO ()
main = do
    setSGR [SetColor Foreground Vivid Cyan]
    escreveNoCentro "Guerras Civis"
    linhaStatusPlayer "vitor" "assasino" "morto"
    linhaSeparacao
    mensagemUsuario "hello darkness my old friend"
    menuSelecao
    setSGR [Reset];
	main


