module Check (verificarXeque, verificarXequeAposMovimento) where

import Data.List (find)
import Tabuleiro (Cor, Peca (..), Posicao, Tabuleiro)
import Utils (charToPeca, corPeca)
import ValidacaoMovimento (movimentoValido)

-- Função para verificar se uma posição está sob ataque por qualquer peça adversária
estaSobAtaque :: Tabuleiro -> Posicao -> Cor -> Bool
estaSobAtaque tab pos cor =
    any (\((x, y), peca) -> corPeca peca /= cor && movimentoValido tab peca (x, y) pos) (todasPecas tab)

-- Função para verificar se o rei de uma determinada cor está em xeque
verificarXeque :: Tabuleiro -> Cor -> Bool
verificarXeque tab cor =
    case find (\(_, peca) -> corPeca peca == cor && tipoPeca peca == Rei cor) (todasPecas tab) of
        Just (pos, _) -> estaSobAtaque tab pos cor -- Verifica se a posição do rei está sob ataque
        Nothing -> False -- Retorna falso se o rei não for encontrado (caso improvável)

-- Função para verificar xeque após um movimento, reutilizando a função verificarXeque
verificarXequeAposMovimento :: Tabuleiro -> Cor -> Bool
verificarXequeAposMovimento = verificarXeque

-- Funções auxiliares

-- Retorna o tipo da peça, preservando sua cor
tipoPeca :: Peca -> Peca
tipoPeca (Rei cor) = Rei cor
tipoPeca (Rainha cor) = Rainha cor
tipoPeca (Torre cor) = Torre cor
tipoPeca (Bispo cor) = Bispo cor
tipoPeca (Cavalo cor) = Cavalo cor
tipoPeca (Peao cor) = Peao cor

-- Retorna uma lista de todas as peças no tabuleiro com suas posições
todasPecas :: Tabuleiro -> [(Posicao, Peca)]
todasPecas tab =
    [((x, y), charToPeca (tab !! y !! x)) | x <- [0 .. 7], y <- [0 .. 7], tab !! y !! x /= ' ']
