module Check (verificarXeque, verificarXequeAposMovimento) where

import Data.List (find)
import Tabuleiro (Cor, Peca (..), Posicao, Tabuleiro)
import Utils (charToPeca, corPeca)
import ValidacaoMovimento (movimentoValido)

-- Funcao para verificar se uma posicao esta sob ataque
estaSobAtaque :: Tabuleiro -> Posicao -> Cor -> Bool
estaSobAtaque tab pos cor =
  any (\((x, y), peca) -> corPeca peca /= cor && movimentoValido tab peca (x, y) pos) (todasPecas tab)

-- Funcao para verificar se o rei esta em xeque
verificarXeque :: Tabuleiro -> Cor -> Bool
verificarXeque tab cor =
  case find (\(_, peca) -> corPeca peca == cor && tipoPeca peca == Rei cor) (todasPecas tab) of
    Just (pos, _) -> estaSobAtaque tab pos cor
    Nothing -> False

-- Funcao para Verificar Xeque Apos Movimento
verificarXequeAposMovimento :: Tabuleiro -> Cor -> Bool
verificarXequeAposMovimento = verificarXeque

-- Funcoes auxiliares

tipoPeca :: Peca -> Peca
tipoPeca (Rei cor) = Rei cor
tipoPeca (Rainha cor) = Rainha cor
tipoPeca (Torre cor) = Torre cor
tipoPeca (Bispo cor) = Bispo cor
tipoPeca (Cavalo cor) = Cavalo cor
tipoPeca (Peao cor) = Peao cor

todasPecas :: Tabuleiro -> [(Posicao, Peca)]
todasPecas tab =
  [((x, y), charToPeca (tab !! y !! x)) | x <- [0 .. 7], y <- [0 .. 7], tab !! y !! x /= ' ']