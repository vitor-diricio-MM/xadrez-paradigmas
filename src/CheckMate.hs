module CheckMate (verificarXequeMate) where

import Tabuleiro
import Check (verificarXeque)
import ValidacaoMovimento (movimentoValido)
import Utils (charToPeca, corPeca)
import ProcessarMovimento (moverPeca)  -- Importa a função moverPeca de ProcessarMovimento

-- Função para verificar se o jogador está em xeque-mate
verificarXequeMate :: Tabuleiro -> Cor -> Bool
verificarXequeMate tab cor = 
    let pecas = todasPecasDoJogador tab cor
        movimentosPossiveis = concatMap (\(pos, peca) -> movimentosValidosParaPeca pos peca tab) pecas
    in all (\novoTab -> verificarXeque novoTab cor) (map (executarMovimento tab) movimentosPossiveis)

-- Função para obter todas as peças de um jogador
todasPecasDoJogador :: Tabuleiro -> Cor -> [(Posicao, Peca)]
todasPecasDoJogador tab cor = 
    filter (\(_, peca) -> corPeca peca == cor) (todasPecas tab)

-- Função para obter todos os movimentos válidos para uma peça
movimentosValidosParaPeca :: Posicao -> Peca -> Tabuleiro -> [(Posicao, Posicao)]
movimentosValidosParaPeca inicio peca tab = 
    [(inicio, fim) | x <- [0..7], y <- [0..7], let fim = (x, y), movimentoValido tab peca inicio fim]

-- Função para executar um movimento no tabuleiro e retornar o novo tabuleiro
executarMovimento :: Tabuleiro -> (Posicao, Posicao) -> Tabuleiro
executarMovimento tab (inicio, fim) = 
    moverPeca inicio fim tab  -- Usa a função moverPeca importada de ProcessarMovimento

-- Função para obter todas as peças no tabuleiro
todasPecas :: Tabuleiro -> [(Posicao, Peca)]
todasPecas tab = 
    [((x, y), charToPeca (tab !! y !! x)) | x <- [0..7], y <- [0..7], tab !! y !! x /= ' ']