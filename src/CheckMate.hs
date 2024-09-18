module CheckMate (verificarXequeMate) where

import Check (verificarXeque)
import ProcessarMovimento (moverPeca)
import Tabuleiro (Cor, Peca, Posicao, Tabuleiro)
import Utils (corPeca, todasPecas)
import ValidacaoMovimento (movimentoValido)

-- Verifica se o jogador está em xeque-mate
verificarXequeMate :: Tabuleiro -> Cor -> Bool
verificarXequeMate tab cor =
  let pecas = todasPecasDoJogador tab cor
      movimentosPossiveis = concatMap (\(pos, peca) -> movimentosValidosParaPeca pos peca tab) pecas
   in all ((`verificarXeque` cor) . executarMovimento tab) movimentosPossiveis

-- Obtém todas as peças de um jogador
todasPecasDoJogador :: Tabuleiro -> Cor -> [(Posicao, Peca)]
todasPecasDoJogador tab cor =
  filter (\(_, peca) -> corPeca peca == cor) (todasPecas tab)

-- Obtém todos os movimentos válidos para uma peça
movimentosValidosParaPeca :: Posicao -> Peca -> Tabuleiro -> [(Posicao, Posicao)]
movimentosValidosParaPeca inicio peca tab =
  [(inicio, fim) | x <- [0 .. 7], y <- [0 .. 7], let fim = (x, y), movimentoValido tab peca inicio fim]

-- Executa um movimento no tabuleiro e retorna o novo tabuleiro
executarMovimento :: Tabuleiro -> (Posicao, Posicao) -> Tabuleiro
executarMovimento tab (inicio, fim) =
  moverPeca inicio fim tab