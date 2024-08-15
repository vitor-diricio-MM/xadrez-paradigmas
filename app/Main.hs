import Check (verificarXequeAposMovimento)
import CheckMate (verificarXequeMate)
import ProcessarMovimento (processarMovimento)
import Tabuleiro

estadoInicial :: (Tabuleiro, Cor)
estadoInicial = (tabuleiroInicial, Branca)

alternaJogador :: Cor -> Cor
alternaJogador Branca = Preta
alternaJogador Preta = Branca

loopJogo :: (Tabuleiro, Cor) -> IO ()
loopJogo estado@(tab, cor) = do
    mostrarTabuleiro tab
    putStrLn $ "Vez das " ++ show cor ++ ". Digite seu movimento ou 'sair' para encerrar:"
    entrada <- getLine
    if entrada == "sair"
        then putStrLn "Fim do jogo!"
        else do
            (novoEstado@(_, _), jogoAcabou) <- processarEntrada entrada estado
            if jogoAcabou
                then do
                    putStrLn "Jogo terminado."
                    mostrarTabuleiro tab
                else loopJogo novoEstado

processarEntrada :: String -> (Tabuleiro, Cor) -> IO ((Tabuleiro, Cor), Bool)
processarEntrada entrada (tab, cor) =
    case processarMovimento entrada tab cor of
        Just novoTabuleiro -> do
            let proximaCor = alternaJogador cor
            let emXeque = verificarXequeAposMovimento novoTabuleiro proximaCor
            if emXeque
                then do
                    putStrLn $ "Xeque! " ++ show proximaCor ++ " está em xeque."
                    if verificarXequeMate novoTabuleiro proximaCor
                        then do
                            putStrLn $ show proximaCor ++ " está em xeque-mate!"
                            return ((novoTabuleiro, proximaCor), True)
                        else return ((novoTabuleiro, proximaCor), False)
                else return ((novoTabuleiro, proximaCor), False)
        Nothing -> do
            putStrLn "Movimento inválido!"
            return ((tab, cor), False)

main :: IO ()
main = do
    putStrLn "Vamos iniciar o jogo de Xadrez!"
    loopJogo estadoInicial
