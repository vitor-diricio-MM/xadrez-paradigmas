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

data EstadoJogo = EstadoJogo
  { tabuleiro :: Tabuleiro,
    corAtual :: Cor,
    posicaoSelecionada :: Maybe Posicao,
    capturadasBrancas :: [Peca],
    capturadasPretas :: [Peca],
    mensagem :: String,
    estadoPromocao :: Maybe (Posicao, Cor) -- (Posição do peão para promover, Cor do peão)
  }

estadoInicial :: EstadoJogo
estadoInicial =
  EstadoJogo
    { tabuleiro = tabuleiroInicial,
      corAtual = Branca,
      posicaoSelecionada = Nothing,
      capturadasBrancas = [],
      capturadasPretas = [],
      mensagem = "Turno das brancas",
      estadoPromocao = Nothing
    }

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
    [ (Rei Branca, reiBranco),
      (Rainha Branca, rainhaBranca),
      (Bispo Branca, bispoBranco),
      (Cavalo Branca, cavaloBranco),
      (Torre Branca, torreBranca),
      (Peao Branca, peaoBranco),
      (Rei Preta, reiPreto),
      (Rainha Preta, rainhaPreta),
      (Bispo Preta, bispoPreto),
      (Cavalo Preta, cavaloPreto),
      (Torre Preta, torrePreta),
      (Peao Preta, peaoPreto)
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
desenharEstado imagens estado =
  Pictures $
    desenharTabuleiro imagens estado
      ++ desenharCapturadas imagens (capturadasBrancas estado) (capturadasPretas estado)
      ++ [Translate (-70) 350 $ Scale 0.15 0.15 $ Text (mensagem estado)]
      ++ desenharOpcoesPromocao imagens estado

desenharTabuleiro :: [(Peca, Picture)] -> EstadoJogo -> [Picture]
desenharTabuleiro imagens estado =
  concatMap (desenharLinha imagens (posicaoSelecionada estado) (tabuleiro estado) (corAtual estado)) (zip [0 ..] (tabuleiro estado))

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
          [ Color corCasa (rectangleSolid squareSize squareSize),
            highlight,
            Translate (squareSize / 2 - pieceOffsetX) (squareSize / 2 - pieceOffsetY) pecaPicture
          ]

-- Função atualizada para desenhar as peças capturadas com espaçamento aumentado e colunas ajustadas
desenharCapturadas :: [(Peca, Picture)] -> [Peca] -> [Peca] -> [Picture]
desenharCapturadas imagens capturadasBrancas capturadasPretas =
  let squareSize = 76
      offsetXBrancas = -350
      offsetXPretas = 350
      offsetY = 250
      piecesPerColumn = 6
      spacing = squareSize * 1.2 -- Aumenta o espaçamento entre as imagens
      -- Peças Brancas Capturadas
      capturadasBrancasPictures =
        [ Translate
            (offsetXBrancas - fromIntegral col * spacing)
            (offsetY - fromIntegral row * spacing)
            $ desenharPecaChar imagens peca
          | (index, peca) <- zip [0 ..] capturadasBrancas,
            let row = index `mod` piecesPerColumn,
            let col = index `div` piecesPerColumn
        ]
      -- Peças Pretas Capturadas
      capturadasPretasPictures =
        [ Translate
            (offsetXPretas + fromIntegral col * spacing)
            (offsetY - fromIntegral row * spacing)
            $ desenharPecaChar imagens peca
          | (index, peca) <- zip [0 ..] capturadasPretas,
            let row = index `mod` piecesPerColumn,
            let col = index `div` piecesPerColumn
        ]
   in capturadasBrancasPictures ++ capturadasPretasPictures

desenharOpcoesPromocao :: [(Peca, Picture)] -> EstadoJogo -> [Picture]
desenharOpcoesPromocao imagens estado =
  case estadoPromocao estado of
    Just (_, cor) ->
      let pecasPromocao = [Rainha cor, Torre cor, Bispo cor, Cavalo cor]
          squareSize = 76
          startX = -squareSize * 2
          startY = 0
       in [ Translate (startX + fromIntegral i * squareSize) startY $
              Pictures
                [ Color (greyN 0.8) (rectangleSolid squareSize squareSize),
                  desenharPecaChar imagens peca
                ]
            | (i, peca) <- zip [0 ..] pecasPromocao
          ]
    Nothing -> []

tratarEvento :: Event -> EstadoJogo -> EstadoJogo
-- Evento de promoção pendente
tratarEvento (EventKey (MouseButton LeftButton) Down _ mousePos) estado@(EstadoJogo {estadoPromocao = Just (posPeao, corPeao)}) =
  case identificarPromocao mousePos corPeao of
    Just pecaEscolhida ->
      let novoTabuleiro = promoverPeao (tabuleiro estado) posPeao pecaEscolhida
          novoEstado =
            estado
              { tabuleiro = novoTabuleiro,
                estadoPromocao = Nothing,
                mensagem =
                  if verificarXequeMate novoTabuleiro (alternarCor (corAtual estado))
                    then "Cheque mate!"
                    else
                      if verificarXeque novoTabuleiro (alternarCor (corAtual estado))
                        then "Cheque!"
                        else
                          if alternarCor (corAtual estado) == Branca then "Turno das brancas" else "Turno das pretas",
                corAtual = alternarCor (corAtual estado)
              }
       in novoEstado
    Nothing -> estado
tratarEvento (EventKey (MouseButton LeftButton) Down _ mousePos) estado@(EstadoJogo {estadoPromocao = Nothing, posicaoSelecionada = Nothing}) =
  case mouseParaPosicao mousePos of
    Just pos ->
      let (pecaChar, _) = pecaNaPosicao pos (tabuleiro estado)
          peca = if pecaChar /= ' ' then Just (charToPeca pecaChar) else Nothing
       in case peca of
            Just p ->
              if corPeca p == corAtual estado
                then estado {posicaoSelecionada = Just pos}
                else estado
            Nothing -> estado
    Nothing -> estado
tratarEvento (EventKey (MouseButton LeftButton) Down _ mousePos) estado@(EstadoJogo {estadoPromocao = Nothing, posicaoSelecionada = Just origem}) =
  case mouseParaPosicao mousePos of
    Just destino ->
      let (pecaDestinoChar, _) = pecaNaPosicao destino (tabuleiro estado)
          pecaCapturada = if pecaDestinoChar /= ' ' then Just (charToPeca pecaDestinoChar) else Nothing
          novoTabuleiro = processarMovimento (posicaoParaString origem destino) (tabuleiro estado) (corAtual estado)
       in case novoTabuleiro of
            Just tabAtualizado ->
              -- Verifica se o peão precisa ser promovido
              if peaoPrecisaPromocao tabAtualizado destino (corAtual estado)
                then
                  estado
                    { tabuleiro = tabAtualizado,
                      posicaoSelecionada = Nothing,
                      estadoPromocao = Just (destino, corAtual estado)
                    }
                else
                  let (novasBrancas, novasPretas) = case pecaCapturada of
                        Just p ->
                          if corPeca p == Branca
                            then (p : capturadasBrancas estado, capturadasPretas estado)
                            else (capturadasBrancas estado, p : capturadasPretas estado)
                        Nothing -> (capturadasBrancas estado, capturadasPretas estado)
                      novoCor = alternarCor (corAtual estado)
                      novaMensagem =
                        if verificarXequeMate tabAtualizado novoCor
                          then "Cheque mate!"
                          else
                            if verificarXeque tabAtualizado novoCor
                              then "Cheque!"
                              else if novoCor == Branca then "Turno das brancas" else "Turno das pretas"
                   in estado
                        { tabuleiro = tabAtualizado,
                          corAtual = novoCor,
                          posicaoSelecionada = Nothing,
                          capturadasBrancas = novasBrancas,
                          capturadasPretas = novasPretas,
                          mensagem = novaMensagem
                        }
            Nothing -> estado {posicaoSelecionada = Nothing, mensagem = "Movimento inválido"}
    Nothing -> estado {posicaoSelecionada = Nothing, mensagem = "Movimento inválido"}
tratarEvento _ estado = estado

promoverPeao :: Tabuleiro -> Posicao -> Peca -> Tabuleiro
promoverPeao tab (x, y) peca =
  let pecaChar = pecaParaChar peca
      linhaAtualizada = take x (tab !! y) ++ [pecaChar] ++ drop (x + 1) (tab !! y)
   in take y tab ++ [linhaAtualizada] ++ drop (y + 1) tab

pecaParaChar :: Peca -> Char
pecaParaChar (Rainha Branca) = 'Q'
pecaParaChar (Torre Branca) = 'R'
pecaParaChar (Bispo Branca) = 'B'
pecaParaChar (Cavalo Branca) = 'N'
pecaParaChar (Rainha Preta) = 'q'
pecaParaChar (Torre Preta) = 'r'
pecaParaChar (Bispo Preta) = 'b'
pecaParaChar (Cavalo Preta) = 'n'
pecaParaChar _ = ' ' -- Não deve acontecer

identificarPromocao :: (Float, Float) -> Cor -> Maybe Peca
identificarPromocao (mouseX, mouseY) cor =
  let squareSize = 76
      startX = -squareSize * 2
      startY = 0
      pecasPromocao = [Rainha cor, Torre cor, Bispo cor, Cavalo cor]
      clickedIndex = floor ((mouseX - startX + squareSize / 2) / squareSize)
   in if mouseY >= startY - squareSize / 2 && mouseY <= startY + squareSize / 2 && clickedIndex >= 0 && clickedIndex < 4
        then Just (pecasPromocao !! clickedIndex)
        else Nothing

peaoPrecisaPromocao :: Tabuleiro -> Posicao -> Cor -> Bool
peaoPrecisaPromocao tab (x, y) cor =
  let pecaChar = (tab !! y) !! x
      peca = charToPeca pecaChar
   in case peca of
        Peao c -> (c == Branca && y == 0) || (c == Preta && y == 7)
        _ -> False

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