module AI (getBestMove) where

import CheckMate (verificarXequeMate)
import Data.List (maximumBy, minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import ProcessarMovimento (processarMovimento)
import Tabuleiro (Cor (..), Peca (..), Posicao, Tabuleiro, pecaNaPosicao)
import Utils (charToPeca, corPeca)
import ValidacaoMovimento (movimentoValido)

maxDepth :: Int
maxDepth = 3 -- Profundidade do algoritmo MiniMax

type Score = Int

infiniteScore :: Int
infiniteScore = 100000

getBestMove :: Tabuleiro -> Cor -> IO (Maybe String)
getBestMove tab cor = do
  let moves = generateAllMoves tab cor
  if null moves
    then return Nothing
    else do
      let scoredMoves = [(move, alphaBeta (makeMove tab move cor) (alternarCor cor) (maxDepth - 1) (-infiniteScore) infiniteScore) | move <- moves]
          bestMove = maximumBy (comparing snd) scoredMoves
      return (Just (fst bestMove))

alphaBeta :: Tabuleiro -> Cor -> Int -> Score -> Score -> Score
alphaBeta tab cor depth alpha beta
  | depth == 0 || verificarXequeMate tab cor = evaluateBoard tab cor
  | otherwise =
      let moves = generateAllMoves tab cor
       in if null moves
            then evaluateBoard tab cor
            else abSearch moves alpha beta
  where
    abSearch [] a b = a
    abSearch (move : moves) a b
      | a >= b = a
      | otherwise =
          let tabAfterMove = makeMove tab move cor
              score = -alphaBeta tabAfterMove (alternarCor cor) (depth - 1) (-b) (-a)
              a' = max a score
           in abSearch moves a' b

generateAllMoves :: Tabuleiro -> Cor -> [String]
generateAllMoves tab cor =
  [ posicaoParaString (x1, y1) (x2, y2)
    | x1 <- [0 .. 7],
      y1 <- [0 .. 7],
      x2 <- [0 .. 7],
      y2 <- [0 .. 7],
      let origem = (x1, y1),
      let destino = (x2, y2),
      let (charOrigem, _) = pecaNaPosicao origem tab,
      charOrigem /= ' ',
      corPeca (charToPeca charOrigem) == cor,
      movimentoValido tab (charToPeca charOrigem) origem destino
  ]

makeMove :: Tabuleiro -> String -> Cor -> Tabuleiro
makeMove tab move cor =
  case processarMovimento move tab cor of
    Just novoTab -> novoTab
    Nothing -> tab

evaluateBoard :: Tabuleiro -> Cor -> Score
evaluateBoard tab cor =
  let allPieces = [charToPeca c | c <- concat tab, c /= ' ']
      myPieces = [p | p <- allPieces, corPeca p == cor]
      opponentPieces = [p | p <- allPieces, corPeca p == alternarCor cor]
      myScore = sum (map pieceValue myPieces)
      opponentScore = sum (map pieceValue opponentPieces)
   in myScore - opponentScore

pieceValue :: Peca -> Int
pieceValue (Peao _) = 10
pieceValue (Cavalo _) = 30
pieceValue (Bispo _) = 30
pieceValue (Torre _) = 50
pieceValue (Rainha _) = 90
pieceValue (Rei _) = 900

posicaoParaString :: Posicao -> Posicao -> String
posicaoParaString (x1, y1) (x2, y2) = [indiceParaColuna x1, indiceParaLinha y1, indiceParaColuna x2, indiceParaLinha y2]

indiceParaColuna :: Int -> Char
indiceParaColuna i = toEnum (i + fromEnum 'a')

indiceParaLinha :: Int -> Char
indiceParaLinha i = toEnum (8 - i + fromEnum '0')

alternarCor :: Cor -> Cor
alternarCor Branca = Preta
alternarCor Preta = Branca