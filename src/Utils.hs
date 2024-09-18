module Utils
  ( colunaParaIndice,
    linhaParaIndice,
    pecaNaPosicao',
    charToPeca,
    corPeca,
    alternarCor,
    todasPecas,
  )
where

import Tabuleiro (Cor (..), Peca (..), Posicao, Tabuleiro)

-- Converte uma coluna em caractere para um índice numérico
colunaParaIndice :: Char -> Int
colunaParaIndice col = fromEnum col - fromEnum 'a'

-- Converte uma linha em caractere para um índice numérico
linhaParaIndice :: Char -> Int
linhaParaIndice linha = 8 - (read [linha] :: Int)

-- Retorna a peça na posição especificada do tabuleiro, se houver
pecaNaPosicao' :: Posicao -> Tabuleiro -> Maybe Peca
pecaNaPosicao' (col, lin) tab =
  let linha = tab !! lin
      pecaChar = linha !! col
   in if pecaChar == ' ' then Nothing else Just (charToPeca pecaChar)

-- Converte um caractere para o tipo de peça correspondente
charToPeca :: Char -> Peca
charToPeca 'R' = Torre Branca
charToPeca 'N' = Cavalo Branca
charToPeca 'B' = Bispo Branca
charToPeca 'Q' = Rainha Branca
charToPeca 'K' = Rei Branca
charToPeca 'P' = Peao Branca
charToPeca 'r' = Torre Preta
charToPeca 'n' = Cavalo Preta
charToPeca 'b' = Bispo Preta
charToPeca 'q' = Rainha Preta
charToPeca 'k' = Rei Preta
charToPeca 'p' = Peao Preta
charToPeca ' ' = error "Espaço vazio não deve ser convertido para peça"
charToPeca _ = error "Caracter desconhecido"

-- Retorna a cor de uma peça
corPeca :: Peca -> Cor
corPeca (Rei cor) = cor
corPeca (Rainha cor) = cor
corPeca (Torre cor) = cor
corPeca (Bispo cor) = cor
corPeca (Cavalo cor) = cor
corPeca (Peao cor) = cor

-- Alterna a cor do jogador
alternarCor :: Cor -> Cor
alternarCor Branca = Preta
alternarCor Preta = Branca

-- Retorna uma lista de todas as peças no tabuleiro com suas posições
todasPecas :: Tabuleiro -> [(Posicao, Peca)]
todasPecas tab =
  [((x, y), charToPeca (tab !! y !! x)) | x <- [0 .. 7], y <- [0 .. 7], tab !! y !! x /= ' ']