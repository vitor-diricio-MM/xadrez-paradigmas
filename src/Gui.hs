module Gui (iniciarJogo) where

import Check (verificarXeque)
import CheckMate (verificarXequeMate)
import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Graphics.Gloss.Data.Color ()
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicy)
import ProcessarMovimento (processarMovimento)
import Tabuleiro (Cor (..), Peca (..), Posicao, Tabuleiro, pecaNaPosicao, tabuleiroInicial)
import Utils (charToPeca, corPeca)
import ValidacaoMovimento (movimentoValido)

type EstadoJogo = (Tabuleiro, Cor, Maybe Posicao, [Peca], [Peca], String)

estadoInicial :: EstadoJogo
estadoInicial = (tabuleiroInicial, Branca, Nothing, [], [], "Turno das brancas")

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
        (InWindow "Jogo de Xadrez" (800, 600) (10, 10))
        white
        30
        estadoInicial
        (desenharEstado imagens)
        tratarEvento
        atualizarEstado

desenharEstado :: [(Peca, Picture)] -> EstadoJogo -> Picture
desenharEstado imagens (tab, cor, maybePos, capturadasBrancas, capturadasPretas, mensagem) =
    Pictures $
        concatMap (desenharLinha imagens maybePos tab cor) (zip [0 ..] tab)
            ++ desenharCapturadas imagens capturadasBrancas capturadasPretas
            ++ [Translate (-70) 350 $ Scale 0.15 0.15 $ Text mensagem]

desenharLinha :: [(Peca, Picture)] -> Maybe Posicao -> Tabuleiro -> Cor -> (Int, [Char]) -> [Picture]
desenharLinha imagens maybePos tab cor (y, linha) = map (desenharPeca imagens maybePos tab cor y) (zip [0 ..] linha)

desenharPeca :: [(Peca, Picture)] -> Maybe Posicao -> Tabuleiro -> Cor -> Int -> (Int, Char) -> Picture
desenharPeca imagens maybePos tab cor y (x, pecaChar) =
    let squareSize = 76
        boardOffsetX = 250
        boardOffsetY = 250
        pieceOffsetX = 35
        pieceOffsetY = 35
        pos = (x, y)
        corCasa = if even (x + y) then makeColor 0.47 0.58 0.34 1.0 else makeColor 0.92 0.93 0.82 1.0
        pecaPicture =
            if pecaChar == ' '
                then Blank
                else desenharPecaChar imagens (charToPeca pecaChar)
        highlight = case maybePos of
            Just origem ->
                if origem == pos
                    then Color (makeColor 1 0 0 0.5) (rectangleSolid squareSize squareSize)
                    else
                        if movimentoValido tab (charToPeca (fst (pecaNaPosicao origem tab))) origem pos
                            then Color (makeColor 0 0 1 0.3) (rectangleSolid squareSize squareSize)
                            else Blank
            Nothing -> Blank
     in Translate (fromIntegral x * squareSize - boardOffsetX) (fromIntegral (7 - y) * squareSize - boardOffsetY) $
            Pictures
                [ Color corCasa (rectangleSolid squareSize squareSize)
                , highlight
                , Translate (squareSize / 2 - pieceOffsetX) (squareSize / 2 - pieceOffsetY) pecaPicture
                ]

desenharCapturadas :: [(Peca, Picture)] -> [Peca] -> [Peca] -> [Picture]
desenharCapturadas imagens capturadasBrancas capturadasPretas =
    let squareSize = 76
        offsetXBrancas = -350
        offsetXPretas = 350
        offsetY = 250
     in [ Translate (offsetXBrancas) (fromIntegral i * (-squareSize) + offsetY) $
            desenharPecaChar imagens peca
        | (i, peca) <- zip [0 ..] capturadasBrancas
        ]
            ++ [ Translate (offsetXPretas) (fromIntegral i * (-squareSize) + offsetY) $
                desenharPecaChar imagens peca
               | (i, peca) <- zip [0 ..] capturadasPretas
               ]

tratarEvento :: Event -> EstadoJogo -> EstadoJogo
tratarEvento (EventKey (MouseButton LeftButton) Down _ mousePos) (tab, cor, Nothing, capturadasBrancas, capturadasPretas, mensagem) =
    case mouseParaPosicao mousePos of
        Just pos ->
            let (pecaChar, _) = pecaNaPosicao pos tab
                peca = if pecaChar /= ' ' then Just (charToPeca pecaChar) else Nothing
             in case peca of
                    Just p -> if corPeca p == cor then (tab, cor, Just pos, capturadasBrancas, capturadasPretas, mensagem) else (tab, cor, Nothing, capturadasBrancas, capturadasPretas, mensagem)
                    Nothing -> (tab, cor, Nothing, capturadasBrancas, capturadasPretas, mensagem)
        Nothing -> (tab, cor, Nothing, capturadasBrancas, capturadasPretas, mensagem)
tratarEvento (EventKey (MouseButton LeftButton) Down _ mousePos) (tab, cor, Just origem, capturadasBrancas, capturadasPretas, _) =
    case mouseParaPosicao mousePos of
        Just destino ->
            let (pecaDestinoChar, _) = pecaNaPosicao destino tab
                pecaCapturada = if pecaDestinoChar /= ' ' then Just (charToPeca pecaDestinoChar) else Nothing
                novoTabuleiro = processarMovimento (posicaoParaString origem destino) tab cor
                novoTabuleiroPromovido = promoverPeao novoTabuleiro destino cor
             in case novoTabuleiroPromovido of
                    Just tabAtualizado ->
                        let (novasBrancas, novasPretas) = case pecaCapturada of
                                Just p -> if corPeca p == Branca then (p : capturadasBrancas, capturadasPretas) else (capturadasBrancas, p : capturadasPretas)
                                Nothing -> (capturadasBrancas, capturadasPretas)
                            novoCor = alternarCor cor
                            mensagem =
                                if verificarXequeMate tabAtualizado novoCor
                                    then "Cheque mate!"
                                    else
                                        if verificarXeque tabAtualizado novoCor
                                            then "Cheque!"
                                            else if novoCor == Branca then "Turno das brancas" else "Turno das pretas"
                         in (tabAtualizado, novoCor, Nothing, novasBrancas, novasPretas, mensagem)
                    Nothing -> (tab, cor, Nothing, capturadasBrancas, capturadasPretas, "Movimento inválido")
        Nothing -> (tab, cor, Nothing, capturadasBrancas, capturadasPretas, "Movimento inválido")
tratarEvento _ estado = estado

promoverPeao :: Maybe Tabuleiro -> Posicao -> Cor -> Maybe Tabuleiro
promoverPeao (Just tab) (x, y) cor
    | (y == 0 && cor == Branca) || (y == 7 && cor == Preta) =
        let linhaAtualizada = take x (tab !! y) ++ [if cor == Branca then 'Q' else 'q'] ++ drop (x + 1) (tab !! y)
         in Just (take y tab ++ [linhaAtualizada] ++ drop (y + 1) tab)
promoverPeao tab _ _ = tab

atualizarEstado :: Float -> EstadoJogo -> EstadoJogo
atualizarEstado _ estado = estado

mouseParaPosicao :: (Float, Float) -> Maybe Posicao
mouseParaPosicao (x, y) =
    let pos = (floor ((x + 300) / 75), 7 - floor ((y + 300) / 75))
     in if x >= -300 && x <= 300 && y >= -300 && y <= 300
            then Just pos
            else Nothing

posicaoParaString :: Posicao -> Posicao -> String
posicaoParaString (x1, y1) (x2, y2) = [indiceParaColuna x1, indiceParaLinha y1, indiceParaColuna x2, indiceParaLinha y2]

indiceParaColuna :: Int -> Char
indiceParaColuna i = toEnum (i + fromEnum 'a')

indiceParaLinha :: Int -> Char
indiceParaLinha i = toEnum (8 - i + fromEnum '0')

alternarCor :: Cor -> Cor
alternarCor Branca = Preta
alternarCor Preta = Branca
