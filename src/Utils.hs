module Utils
  ( colunaParaIndice,
    linhaParaIndice,
    pecaNaPosicao',
    charToPeca,
    corPeca,
  )
where

import Tabuleiro (Cor (..), Peca (..), Posicao, Tabuleiro)

-- Funcoes de Conversao de Coordenadas
colunaParaIndice :: Char -> Int
colunaParaIndice col = fromEnum col - fromEnum 'a'

linhaParaIndice :: Char -> Int
linhaParaIndice linha = 8 - (read [linha] :: Int)

-- Funcao para Obter Peca na Posicao
pecaNaPosicao' :: Posicao -> Tabuleiro -> Maybe Peca
pecaNaPosicao' (col, lin) tab =
  let linha = tab !! lin
      pecaChar = linha !! col
   in if pecaChar == ' ' then Nothing else Just (charToPeca pecaChar)

-- Funcao para converter Char para Peca
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
charToPeca _ = error "Caracter desconhecido"

-- Funcao para obter a cor de uma peca
corPeca :: Peca -> Cor
corPeca (Rei cor) = cor
corPeca (Rainha cor) = cor
corPeca (Torre cor) = cor
corPeca (Bispo cor) = cor
corPeca (Cavalo cor) = cor
corPeca (Peao cor) = cor