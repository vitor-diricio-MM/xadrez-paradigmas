module Gui (iniciarJogo) where

import Data.Maybe (fromMaybe)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicy)
import Tabuleiro (Cor (..), Peca (..), Tabuleiro, tabuleiroInicial)
import Utils (charToPeca)

-- Define o tamanho da janela e do tabuleiro
larguraJanela, alturaJanela, offset :: Int
larguraJanela = 600
alturaJanela = 600
offset = 100

-- Define o estado inicial do jogo
estadoInicial :: (Tabuleiro, Cor)
estadoInicial = (tabuleiroInicial, Branca)

-- Função para carregar imagens das peças
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

-- Função auxiliar para carregar uma imagem
carregarImagem :: FilePath -> IO Picture
carregarImagem caminho = do
    maybeImg <- loadJuicy caminho
    return $ fromMaybe (error $ "Erro ao carregar imagem: " ++ caminho) maybeImg

-- Função para desenhar o caractere da peça
desenharPecaChar :: [(Peca, Picture)] -> Peca -> Picture
desenharPecaChar imagens peca = fromMaybe Blank (lookup peca imagens)

-- Função para iniciar o jogo
iniciarJogo :: IO ()
iniciarJogo = do
    imagens <- carregarImagens
    play
        (InWindow "Jogo de Xadrez" (larguraJanela, alturaJanela) (offset, offset))
        white
        30
        estadoInicial
        (desenharEstado imagens)
        tratarEvento
        atualizarEstado

-- Função para desenhar o estado do jogo
desenharEstado :: [(Peca, Picture)] -> (Tabuleiro, Cor) -> Picture
desenharEstado imagens (tab, _) = Pictures $ concatMap (desenharLinha imagens) (zip [0 ..] tab)

-- Função para desenhar uma linha do tabuleiro
desenharLinha :: [(Peca, Picture)] -> (Int, [Char]) -> [Picture]
desenharLinha imagens (y, linha) = map (desenharPeca imagens y) (zip [0 ..] linha)

-- Função para desenhar uma peça
desenharPeca :: [(Peca, Picture)] -> Int -> (Int, Char) -> Picture
desenharPeca imagens y (x, pecaChar) =
    let cor = if even (x + y) then white else black
        pecaPicture =
            if pecaChar == ' '
                then Blank
                else desenharPecaChar imagens (charToPeca pecaChar)
     in Translate (fromIntegral x * 75 - 300) (fromIntegral y * 75 - 300) $
            Pictures [Color cor (rectangleSolid 75 75), pecaPicture]

-- Função para tratar eventos (a ser implementada)
tratarEvento :: Event -> (Tabuleiro, Cor) -> (Tabuleiro, Cor)
tratarEvento _ estado = estado

-- Função para atualizar o estado do jogo (a ser implementada)
atualizarEstado :: Float -> (Tabuleiro, Cor) -> (Tabuleiro, Cor)
atualizarEstado _ estado = estado
