module Check (verificarXeque, verificarXequeAposMovimento) where

import Data.List (find)
import Tabuleiro (Cor, Peca (..), Posicao, Tabuleiro)
import Utils (corPeca, todasPecas)
import ValidacaoMovimento (movimentoValido)

-- Verifica se uma posição está sob ataque por qualquer peça adversária
estaSobAtaque :: Tabuleiro -> Posicao -> Cor -> Bool
estaSobAtaque tab pos cor =
  any (\((x, y), peca) -> corPeca peca /= cor && movimentoValido tab peca (x, y) pos) (todasPecas tab)

-- Verifica se o rei de uma determinada cor está em xeque
verificarXeque :: Tabuleiro -> Cor -> Bool
verificarXeque tab cor =
  case find (\(_, peca) -> corPeca peca == cor && tipoPeca peca == Rei cor) (todasPecas tab) of
    Just (pos, _) -> estaSobAtaque tab pos cor -- Verifica se a posição do rei está sob ataque
    Nothing -> False -- Retorna falso se o rei não for encontrado (caso improvável)

-- Verifica xeque após um movimento, reutilizando a função verificarXeque
verificarXequeAposMovimento :: Tabuleiro -> Cor -> Bool
verificarXequeAposMovimento = verificarXeque

-- Retorna o tipo da peça, preservando sua cor
tipoPeca :: Peca -> Peca
tipoPeca (Rei cor) = Rei cor
tipoPeca (Rainha cor) = Rainha cor
tipoPeca (Torre cor) = Torre cor
tipoPeca (Bispo cor) = Bispo cor
tipoPeca (Cavalo cor) = Cavalo cor
tipoPeca (Peao cor) = Peao cor
