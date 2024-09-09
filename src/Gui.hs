module Gui (iniciarJogo) where

import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Graphics.Gloss.Data.Color ()
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicy)
import ProcessarMovimento (processarMovimento)
import Tabuleiro (Cor (..), Peca (..), Posicao, Tabuleiro, pecaNaPosicao, tabuleiroInicial)
import Utils (charToPeca, corPeca)

type EstadoJogo = (Tabuleiro, Cor, Maybe Posicao)

estadoInicial :: EstadoJogo
estadoInicial = (tabuleiroInicial, Branca, Nothing)

carregarImagens :: IO [(Peca, Picture)]
carregarImagens = do
    reiBranco <- carregarImagem "imagens/rei_branco.png"
    rainhaBranca <- carregarImagem "imagens/rainha_branca.png"
    bispoBranco <- carregarImagem "imagens/bispo_branco.png"
    cavaloBranco <- carregarImagem "imagens/cavalo_branco.png"
    torreBranca <- carregarImagem "imagens/torre_branca.png"
    peaoBranco <- carregarImagem "imagens/peao_branco.png"
    reiPreto <- carregarImagem "imagens/rei_preto.png"
    rainhaPreta <- carregarImagem "imagens/rainha_preta.png"
    bispoPreto <- carregarImagem "imagens/bispo_preto.png"
    cavaloPreto <- carregarImagem "imagens/cavalo_preto.png"
    torrePreta <- carregarImagem "imagens/torre_preta.png"
    peaoPreto <- carregarImagem "imagens/peao_preto.png"
    return
        [ (Rei Branca, reiBranco)
        , (Rainha Branca, rainhaBranca)
        , (Bispo Branca, bispoBranco)
        , (Cavalo Branca, cavaloBranco)
        , (Torre Branca, torreBranca)
        , (Peao Branca, peaoBranco)
        , (Rei Preta, reiPreto)
        , (Rainha Preta, rainhaPreta)
        , (Bispo Preta, bispoPreto)
        , (Cavalo Preta, cavaloPreto)
        , (Torre Preta, torrePreta)
        , (Peao Preta, peaoPreto)
        ]

carregarImagem :: FilePath -> IO Picture
carregarImagem caminho = do
    maybeImg <- loadJuicy caminho
    return $ fromMaybe (error $ "Erro ao carregar imagem: " ++ caminho) maybeImg

desenharPecaChar :: [(Peca, Picture)] -> Peca -> Picture
desenharPecaChar imagens peca =
    let squareSize = 76
        scaleFactor = (squareSize * 0.6) / 512
        originalPicture = fromMaybe Blank (lookup peca imagens)
     in Scale scaleFactor scaleFactor originalPicture

iniciarJogo :: IO ()
iniciarJogo = do
    imagens <- carregarImagens
    play
        (InWindow "Jogo de Xadrez" (600, 600) (10, 10))
        white
        30
        estadoInicial
        (desenharEstado imagens)
        tratarEvento
        atualizarEstado

desenharEstado :: [(Peca, Picture)] -> EstadoJogo -> Picture
desenharEstado imagens (tab, _, _) =
    Pictures $ concatMap (desenharLinha imagens) (zip [0 ..] tab)

desenharLinha :: [(Peca, Picture)] -> (Int, [Char]) -> [Picture]
desenharLinha imagens (y, linha) = map (desenharPeca imagens y) (zip [0 ..] linha)

desenharPeca :: [(Peca, Picture)] -> Int -> (Int, Char) -> Picture
desenharPeca imagens y (x, pecaChar) =
    let squareSize = 76
        boardOffsetX = 250
        boardOffsetY = 250
        pieceOffsetX = 35
        pieceOffsetY = 35
        cor = if even (x + y) then makeColor 0.47 0.58 0.34 1.0 else makeColor 0.92 0.93 0.82 1.0
        pecaPicture =
            if pecaChar == ' '
                then Blank
                else desenharPecaChar imagens (charToPeca pecaChar)
     in Translate (fromIntegral x * squareSize - boardOffsetX) (fromIntegral (7 - y) * squareSize - boardOffsetY) $
            Pictures
                [ Color cor (rectangleSolid squareSize squareSize)
                , Translate (squareSize / 2 - pieceOffsetX) (squareSize / 2 - pieceOffsetY) pecaPicture
                ]

tratarEvento :: Event -> EstadoJogo -> EstadoJogo
tratarEvento (EventKey (MouseButton LeftButton) Down _ mousePos) (tab, cor, Nothing) =
    let pos = mouseParaPosicao mousePos
        (pecaChar, _) = pecaNaPosicao pos tab
        peca = if pecaChar /= ' ' then Just (charToPeca pecaChar) else Nothing
     in case peca of
            Just p -> if corPeca p == cor then (tab, cor, Just pos) else (tab, cor, Nothing)
            Nothing -> (tab, cor, Nothing)
tratarEvento (EventKey (MouseButton LeftButton) Down _ mousePos) (tab, cor, Just origem) =
    let destino = mouseParaPosicao mousePos
        novoTabuleiro = processarMovimento (posicaoParaString origem destino) tab cor
     in case novoTabuleiro of
            Just tabAtualizado -> (tabAtualizado, alternarCor cor, Nothing)
            Nothing -> (tab, cor, Nothing)
tratarEvento _ estado = estado

atualizarEstado :: Float -> EstadoJogo -> EstadoJogo
atualizarEstado _ estado = estado

mouseParaPosicao :: (Float, Float) -> Posicao
mouseParaPosicao (x, y) = (floor ((x + 300) / 75), 7 - floor ((y + 300) / 75))

posicaoParaString :: Posicao -> Posicao -> String
posicaoParaString (x1, y1) (x2, y2) = [indiceParaColuna x1, indiceParaLinha y1, indiceParaColuna x2, indiceParaLinha y2]

indiceParaColuna :: Int -> Char
indiceParaColuna i = toEnum (i + fromEnum 'a')

indiceParaLinha :: Int -> Char
indiceParaLinha i = toEnum (8 - i + fromEnum '0')

alternarCor :: Cor -> Cor
alternarCor Branca = Preta
alternarCor Preta = Branca
