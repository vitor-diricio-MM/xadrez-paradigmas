module ValidacaoMovimento
  ( movimentoValido,
    caminhoLivre,
    caminhoDiagonal,
    roqueValido,
  )
where

import Data.Maybe (fromJust, isJust, isNothing)
import Tabuleiro (Cor (..), Peca (..), Posicao, Tabuleiro)
import Utils (corPeca, pecaNaPosicao')

-- Função para verificar se o movimento de uma peça é válido
movimentoValido :: Tabuleiro -> Peca -> Posicao -> Posicao -> Bool
movimentoValido tab peca inicio fim =
  let pecaDestino = pecaNaPosicao' fim tab -- Obtém a peça na posição de destino
   in case peca of
        Rei _ -> (abs (fst inicio - fst fim) <= 1 && abs (snd inicio - snd fim) <= 1 && not (mesmaCor peca pecaDestino)) || roqueValido tab peca inicio fim -- Movimento de uma casa em qualquer direção
        Peao cor -> movimentoPeaoValido cor inicio fim tab
        Cavalo _ ->
          ( (abs (fst inicio - fst fim) == 1 && abs (snd inicio - snd fim) == 2)
              || (abs (fst inicio - fst fim) == 2 && abs (snd inicio - snd fim) == 1)
          )
            && not (mesmaCor peca pecaDestino)
        Torre _ -> (fst inicio == fst fim || snd inicio == snd fim) && caminhoLivre inicio fim tab && not (mesmaCor peca pecaDestino) -- Movimento em linha reta
        Bispo _ -> abs (fst inicio - fst fim) == abs (snd inicio - snd fim) && caminhoLivre inicio fim tab && not (mesmaCor peca pecaDestino) -- Movimento em diagonal
        Rainha _ -> (fst inicio == fst fim || snd inicio == snd fim || abs (fst inicio - fst fim) == abs (snd inicio - snd fim)) && caminhoLivre inicio fim tab && not (mesmaCor peca pecaDestino) -- Movimento em linha reta ou diagonal
  where
    -- Verifica se duas peças são da mesma cor
    mesmaCor :: Peca -> Maybe Peca -> Bool
    mesmaCor _ Nothing = False
    mesmaCor p1 (Just p2) = corPeca p1 == corPeca p2

-- Função para verificar se o roque é válido
roqueValido :: Tabuleiro -> Peca -> Posicao -> Posicao -> Bool
roqueValido tab (Rei cor) (x1, y1) (x2, y2)
  | cor == Branca && (x1, y1) == (4, 7) && (x2, y2) == (6, 7) = caminhoLivre (4, 7) (7, 7) tab && isJust (pecaNaPosicao' (7, 7) tab) && pecaNaPosicao' (7, 7) tab == Just (Torre Branca)
  | cor == Branca && (x1, y1) == (4, 7) && (x2, y2) == (2, 7) = caminhoLivre (4, 7) (0, 7) tab && isJust (pecaNaPosicao' (0, 7) tab) && pecaNaPosicao' (0, 7) tab == Just (Torre Branca)
  | cor == Preta && (x1, y1) == (4, 0) && (x2, y2) == (6, 0) = caminhoLivre (4, 0) (7, 0) tab && isJust (pecaNaPosicao' (7, 0) tab) && pecaNaPosicao' (7, 0) tab == Just (Torre Preta)
  | cor == Preta && (x1, y1) == (4, 0) && (x2, y2) == (2, 0) = caminhoLivre (4, 0) (0, 0) tab && isJust (pecaNaPosicao' (0, 0) tab) && pecaNaPosicao' (0, 0) tab == Just (Torre Preta)
  | otherwise = False
roqueValido _ _ _ _ = False

-- Função para verificar se o movimento do peão é válido
movimentoPeaoValido :: Cor -> Posicao -> Posicao -> Tabuleiro -> Bool
movimentoPeaoValido cor (x1, y1) (x2, y2) tab =
  let pecaDestino = pecaNaPosicao' (x2, y2) tab
   in case cor of
        Branca ->
          (x1 == x2 && y2 == y1 - 1 && isNothing pecaDestino) -- Movimento de uma casa para frente
            || (x1 == x2 && y1 == 6 && y2 == 4 && all (isNothing . flip pecaNaPosicao' tab) [(x1, 5), (x1, 4)]) -- Movimento de duas casas na primeira jogada
            || (abs (x1 - x2) == 1 && y2 == y1 - 1 && isJust pecaDestino && corPeca (fromJust pecaDestino) == Preta) -- Captura de peça adversária
        Preta ->
          (x1 == x2 && y2 == y1 + 1 && isNothing pecaDestino) -- Movimento de uma casa para frente
            || (x1 == x2 && y1 == 1 && y2 == 3 && all (isNothing . flip pecaNaPosicao' tab) [(x1, 2), (x1, 3)]) -- Movimento de duas casas na primeira jogada
            || (abs (x1 - x2) == 1 && y2 == y1 + 1 && isJust pecaDestino && corPeca (fromJust pecaDestino) == Branca) -- Captura de peça adversária

-- Função para verificar se o caminho entre duas posições está livre
caminhoLivre :: Posicao -> Posicao -> Tabuleiro -> Bool
caminhoLivre (x1, y1) (x2, y2) tab
  | x1 == x2 = all (\y -> isNothing (pecaNaPosicao' (x1, y) tab)) [min y1 y2 + 1 .. max y1 y2 - 1]
  | y1 == y2 = all (\x -> isNothing (pecaNaPosicao' (x, y1) tab)) [min x1 x2 + 1 .. max x1 x2 - 1]
  | otherwise = all (\(x, y) -> isNothing (pecaNaPosicao' (x, y) tab)) $ caminhoDiagonal (x1, y1) (x2, y2)

-- Função para obter as posições no caminho diagonal entre duas posições
caminhoDiagonal :: Posicao -> Posicao -> [Posicao]
caminhoDiagonal (x1, y1) (x2, y2) =
  tail $ takeWhile (/= (x2, y2)) $ iterate next (x1, y1)
  where
    next (x, y) = (if x2 > x1 then succ x else pred x, if y2 > y1 then succ y else pred y)