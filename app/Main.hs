import Check (verificarXequeAposMovimento)
import CheckMate (verificarXequeMate)
import ProcessarMovimento (processarMovimento)
import Tabuleiro
  ( Cor (..),
    Tabuleiro,
    mostrarTabuleiro,
    tabuleiroInicial,
  )

-- Define o estado inicial do jogo com o tabuleiro inicial e a cor das peças que começam jogando
estadoInicial :: (Tabuleiro, Cor)
estadoInicial = (tabuleiroInicial, Branca)

-- Alterna a cor do jogador atual
alternaJogador :: Cor -> Cor
alternaJogador Branca = Preta
alternaJogador Preta = Branca

-- Loop principal do jogo, que continua até que o jogo termine ou o jogador digite 'sair'
loopJogo :: (Tabuleiro, Cor) -> IO ()
loopJogo estado@(tab, cor) = do
  mostrarTabuleiro tab -- Exibe o tabuleiro atual
  putStrLn $ "Vez das " ++ show cor ++ ". Digite seu movimento ou 'sair' para encerrar:"
  entrada <- getLine -- Lê a entrada do jogador
  if entrada == "sair"
    then putStrLn "Fim do jogo!" -- Encerra o jogo se o jogador digitar 'sair'
    else do
      (novoEstado@(_, _), jogoAcabou) <- processarEntrada entrada estado
      if jogoAcabou
        then do
          putStrLn "Jogo terminado."
          mostrarTabuleiro tab -- Exibe o tabuleiro final
        else loopJogo novoEstado -- Continua o loop do jogo com o novo estado

-- Processa a entrada do jogador e atualiza o estado do jogo
processarEntrada :: String -> (Tabuleiro, Cor) -> IO ((Tabuleiro, Cor), Bool)
processarEntrada entrada (tab, cor) =
  case processarMovimento entrada tab cor of
    Just novoTabuleiro -> do
      let proximaCor = alternaJogador cor -- Alterna para o próximo jogador
      let emXeque = verificarXequeAposMovimento novoTabuleiro proximaCor
      if emXeque
        then do
          putStrLn $ "Xeque! " ++ show proximaCor ++ " esta em xeque."
          if verificarXequeMate novoTabuleiro proximaCor
            then do
              putStrLn $ show proximaCor ++ " esta em xeque-mate!"
              return ((novoTabuleiro, proximaCor), True) -- Indica que o jogo acabou
            else return ((novoTabuleiro, proximaCor), False) -- Continua o jogo
        else return ((novoTabuleiro, proximaCor), False) -- Continua o jogo
    Nothing -> do
      putStrLn "Movimento invalido!" -- Informa que o movimento foi inválido
      return ((tab, cor), False) -- Retorna o estado atual sem alterações

-- Função principal que inicia o jogo
main :: IO ()
main = do
  putStrLn "Vamos iniciar o jogo de Xadrez!"
  loopJogo estadoInicial -- Inicia o loop do jogo com o estado inicial