module CheckMate (verificarXequeMate) where

import Check (verificarXeque)
import ProcessarMovimento (moverPeca) -- Importa a funcao moverPeca de ProcessarMovimento
import Tabuleiro (Cor, Peca, Posicao, Tabuleiro)
import Utils (charToPeca, corPeca)
import ValidacaoMovimento (movimentoValido)

-- Funcao para verificar se o jogador esta em xeque-mate
verificarXequeMate :: Tabuleiro -> Cor -> Bool
verificarXequeMate tab cor =
  let pecas = todasPecasDoJogador tab cor
      movimentosPossiveis = concatMap (\(pos, peca) -> movimentosValidosParaPeca pos peca tab) pecas
   in all ((`verificarXeque` cor) . executarMovimento tab) movimentosPossiveis

-- Funcao para obter todas as pecas de um jogador
todasPecasDoJogador :: Tabuleiro -> Cor -> [(Posicao, Peca)]
todasPecasDoJogador tab cor =
  filter (\(_, peca) -> corPeca peca == cor) (todasPecas tab)

-- Funcao para obter todos os movimentos validos para uma peca
movimentosValidosParaPeca :: Posicao -> Peca -> Tabuleiro -> [(Posicao, Posicao)]
movimentosValidosParaPeca inicio peca tab =
  [(inicio, fim) | x <- [0 .. 7], y <- [0 .. 7], let fim = (x, y), movimentoValido tab peca inicio fim]

-- Funcao para executar um movimento no tabuleiro e retornar o novo tabuleiro
executarMovimento :: Tabuleiro -> (Posicao, Posicao) -> Tabuleiro
executarMovimento tab (inicio, fim) =
  moverPeca inicio fim tab -- Usa a funcao moverPeca importada de ProcessarMovimento

-- Funcao para obter todas as pecas no tabuleiro
todasPecas :: Tabuleiro -> [(Posicao, Peca)]
todasPecas tab =
  [((x, y), charToPeca (tab !! y !! x)) | x <- [0 .. 7], y <- [0 .. 7], tab !! y !! x /= ' ']