module Tabuleiro (
    Tabuleiro,
    tabuleiroInicial,
    mostrarTabuleiro,
    legendas,
    Peca (..),
    Cor (..),
    Posicao,
    pecaNaPosicao,
) where

import Control.Monad
import Data.Char (isLower, isUpper) -- Adicione esta linha

type Tabuleiro = [[Char]]
type Posicao = (Int, Int)
data Cor = Branca | Preta deriving (Show, Eq)

data Peca
    = Rei Cor
    | Rainha Cor
    | Bispo Cor
    | Cavalo Cor
    | Torre Cor
    | Peao Cor
    deriving (Eq, Show)

tabuleiroInicial :: Tabuleiro
tabuleiroInicial =
    [ "rnbqkbnr"
    , "pppppppp"
    , "        "
    , "        "
    , "        "
    , "        "
    , "PPPPPPPP"
    , "RNBQKBNR"
    ]

-- Função que será utilizada para mostrar o tabuleiro na tela
mostrarTabuleiro :: Tabuleiro -> IO ()
mostrarTabuleiro tab = do
    putStrLn "   a b c d e f g h"
    putStrLn "  -----------------"
    forM_ (zip ([8, 7 .. 1] :: [Int]) tab) $ \(num, linha) -> do
        putStr $ show num ++ " |"
        mapM_ (\c -> putStr $ ' ' : c : []) linha
        putStrLn (" | " ++ show num)
    putStrLn "  -----------------"
    putStrLn "   a b c d e f g h"

-- Legenda com as peças do jogo
legendas :: IO ()
legendas = do
    putStrLn "\nLegendas:"
    putStrLn "R/r - Torre (Rook)"
    putStrLn "N/n - Cavalo (Knight)"
    putStrLn "B/b - Bispo (Bishop)"
    putStrLn "Q/q - Rainha (Queen)"
    putStrLn "K/k - Rei (King)"
    putStrLn "P/p - Peão (Pawn)"
    putStrLn "(Letras maiúsculas: brancas, letras minúsculas: pretas)"

-- Função para obter a peça em uma determinada posição no tabuleiro
pecaNaPosicao :: Posicao -> Tabuleiro -> (Char, Maybe Cor)
pecaNaPosicao (col, lin) tab =
    let linha = tab !! lin
        peca = linha !! col
     in (peca, corPecaFromChar peca)

-- Função auxiliar para obter a cor de uma peça a partir do caractere
corPecaFromChar :: Char -> Maybe Cor
corPecaFromChar c
    | isUpper c = Just Branca
    | isLower c = Just Preta
    | otherwise = Nothing
