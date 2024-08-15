module Check (verificarXeque, verificarXequeAposMovimento) where

import Tabuleiro
import Data.List (find)
import Utils (charToPeca, corPeca)
import ValidacaoMovimento (movimentoValido)

-- Função para verificar se uma posição está sob ataque
estaSobAtaque :: Tabuleiro -> Posicao -> Cor -> Bool
estaSobAtaque tab pos cor =
  any (\((x, y), peca) -> corPeca peca /= cor && movimentoValido tab peca (x, y) pos) (todasPecas tab)

-- Função para verificar se o rei está em xeque
verificarXeque :: Tabuleiro -> Cor -> Bool
verificarXeque tab cor =
  case find (\(_, peca) -> corPeca peca == cor && tipoPeca peca == Rei cor) (todasPecas tab) of
    Just (pos, _) -> estaSobAtaque tab pos cor
    Nothing -> False

-- Função para Verificar Xeque Após Movimento
verificarXequeAposMovimento :: Tabuleiro -> Cor -> Bool
verificarXequeAposMovimento tab cor = verificarXeque tab cor

-- Funções auxiliares

tipoPeca :: Peca -> Peca
tipoPeca (Rei cor) = Rei cor
tipoPeca (Rainha cor) = Rainha cor
tipoPeca (Torre cor) = Torre cor
tipoPeca (Bispo cor) = Bispo cor
tipoPeca (Cavalo cor) = Cavalo cor
tipoPeca (Peao cor) = Peao cor

todasPecas :: Tabuleiro -> [(Posicao, Peca)]
todasPecas tab = 
    [((x, y), charToPeca (tab !! y !! x)) | x <- [0..7], y <- [0..7], tab !! y !! x /= ' ']