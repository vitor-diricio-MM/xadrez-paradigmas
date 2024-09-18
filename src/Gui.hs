module Gui (iniciarJogo) where

import AI (getBestMove)
import Check (verificarXeque)
import CheckMate (verificarXequeMate)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Graphics.Gloss
  ( Display (InWindow),
    Picture (Blank, Color, Pictures, Scale, Text, Translate),
    black,
    greyN,
    makeColor,
    rectangleSolid,
    red,
    white,
  )
import Graphics.Gloss.Interface.IO.Game
  ( Event (EventKey),
    Key (MouseButton),
    KeyState (Down),
    MouseButton (LeftButton),
    playIO,
  )
import Graphics.Gloss.Juicy (loadJuicy)
import ProcessarMovimento (processarMovimento)
import System.Exit (exitSuccess)
import Tabuleiro (Cor (..), Peca (..), Posicao, Tabuleiro, pecaNaPosicao, tabuleiroInicial)
import Utils (charToPeca, corPeca)
import ValidacaoMovimento (movimentoValido)

data EstadoJogo = EstadoJogo
  { estadoAtual :: Estado,
    tabuleiro :: Tabuleiro,
    corAtual :: Cor,
    posicaoSelecionada :: Maybe Posicao,
    capturadasBrancas :: [Peca],
    capturadasPretas :: [Peca],
    mensagem :: String,
    estadoPromocao :: Maybe (Posicao, Cor), -- (Posição do peão para promover, Cor do peão)
    tempoFinalizacao :: Maybe Float -- Tempo restante para fechar o jogo após cheque mate
  }

data Estado = Menu | Jogando | UmJogador | GameOver deriving (Eq)

estadoInicial :: EstadoJogo
estadoInicial =
  EstadoJogo
    { estadoAtual = Menu,
      tabuleiro = tabuleiroInicial,
      corAtual = Branca,
      posicaoSelecionada = Nothing,
      capturadasBrancas = [],
      capturadasPretas = [],
      mensagem = "Bem-vindo ao Xadrez",
      estadoPromocao = Nothing,
      tempoFinalizacao = Nothing
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
  playIO
    (InWindow "Jogo de Xadrez" (800, 600) (10, 10))
    white
    30
    estadoInicial
    (desenharEstado imagens)
    tratarEvento
    atualizarEstado

desenharEstado :: [(Peca, Picture)] -> EstadoJogo -> IO Picture
desenharEstado imagens estado =
  case estadoAtual estado of
    Menu -> return $ desenharMenu estado
    Jogando -> return $ desenharJogo imagens estado
    UmJogador -> return $ desenharJogo imagens estado
    GameOver -> return $ desenharGameOver estado

desenharMenu :: EstadoJogo -> Picture
desenharMenu _ =
  Pictures
    [ Translate (-150) 150 $ Scale 0.2 0.2 $ Text "Bem-vindo ao Xadrez",
      desenharBotao 0 50 "Um jogador",
      desenharBotao 0 (-20) "Dois jogadores",
      desenharBotao 0 (-90) "Sair"
    ]

desenharJogo :: [(Peca, Picture)] -> EstadoJogo -> Picture
desenharJogo imagens estadoJogo =
  Pictures $
    desenharTabuleiro imagens estadoJogo
      ++ desenharCapturadas imagens (capturadasBrancas estadoJogo) (capturadasPretas estadoJogo)
      ++ [Translate (-70) 350 $ Scale 0.15 0.15 $ Text (mensagem estadoJogo)]
      ++ desenharOpcoesPromocao imagens estadoJogo
      ++ [desenharBotao 0 (-420) "Voltar"]

desenharGameOver :: EstadoJogo -> Picture
desenharGameOver estado =
  Pictures
    [Translate (-150) 0 $ Scale 0.3 0.3 $ Color red $ Text "Cheque Mate"]

desenharBotao :: Float -> Float -> String -> Picture
desenharBotao x y texto =
  let escala = 0.2
      larguraBotao = 300 -- Largura fixa
      alturaBotao = 45 -- Altura fixa
      larguraTexto = fromIntegral (length texto) * 30 * escala
      xTexto = x - larguraTexto -- Centraliza o texto horizontalmente
      yTexto = y - (30 * escala) -- Centraliza o texto verticalmente
   in Pictures
        [ Translate x y $ Color (greyN 0.8) $ rectangleSolid larguraBotao alturaBotao,
          Translate xTexto yTexto $ Scale escala escala $ Color black $ Text texto
        ]

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

desenharCapturadas :: [(Peca, Picture)] -> [Peca] -> [Peca] -> [Picture]
desenharCapturadas imagens capturadasBrancas capturadasPretas =
  let squareSize = 76
      offsetXBrancas = -350
      offsetXPretas = 350
      offsetY = 250
      piecesPerColumn = 6
      spacing = squareSize * 1.2
      capturadasBrancasPictures =
        [ Translate
            (offsetXBrancas - fromIntegral col * spacing)
            (offsetY - fromIntegral row * spacing)
            $ desenharPecaChar imagens peca
          | (index, peca) <- zip [0 ..] capturadasBrancas,
            let row = index `mod` piecesPerColumn,
            let col = index `div` piecesPerColumn
        ]
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

tratarEvento :: Event -> EstadoJogo -> IO EstadoJogo
tratarEvento (EventKey (MouseButton LeftButton) Down _ (mx, my)) estado@(EstadoJogo {estadoAtual = Menu}) =
  if botaoClicado 0 50 "Um jogador" mx my
    then return $ estado {estadoAtual = UmJogador, mensagem = "Turno das brancas"}
    else
      if botaoClicado 0 (-20) "Dois jogadores" mx my
        then return $ estado {estadoAtual = Jogando, mensagem = "Turno das brancas"}
        else
          if botaoClicado 0 (-90) "Sair" mx my
            then exitSuccess
            else return estado
tratarEvento evento estado@(EstadoJogo {estadoAtual = Jogando}) =
  if isCliqueVoltar evento
    then return $ estado {estadoAtual = Menu}
    else tratarEventoJogo evento estado
tratarEvento evento estado@(EstadoJogo {estadoAtual = UmJogador}) =
  if isCliqueVoltar evento
    then return $ estado {estadoAtual = Menu}
    else tratarEventoJogo evento estado
tratarEvento _ estado@(EstadoJogo {estadoAtual = GameOver}) = return estado
tratarEvento _ estado = return estado

isCliqueVoltar :: Event -> Bool
isCliqueVoltar (EventKey (MouseButton LeftButton) Down _ (mx, my)) = botaoClicado 0 (-420) "Voltar" mx my
isCliqueVoltar _ = False

botaoClicado :: Float -> Float -> String -> Float -> Float -> Bool
botaoClicado x y _ mx my =
  let larguraBotao = 300
      alturaBotao = 45
      x1 = x - larguraBotao / 2
      x2 = x + larguraBotao / 2
      y1 = y - alturaBotao / 2
      y2 = y + alturaBotao / 2
   in mx >= x1 && mx <= x2 && my >= y1 && my <= y2

tratarEventoJogo :: Event -> EstadoJogo -> IO EstadoJogo
tratarEventoJogo (EventKey (MouseButton LeftButton) Down _ mousePos) estado@(EstadoJogo {estadoPromocao = Just (posPeao, corPeao), estadoAtual = _}) =
  case identificarPromocao mousePos corPeao of
    Just pecaEscolhida -> do
      let novoTabuleiro = promoverPeao (tabuleiro estado) posPeao pecaEscolhida
          verificaCheckMate = verificarXequeMate novoTabuleiro (alternarCor corPeao)
          novoEstado =
            if verificaCheckMate
              then
                estado
                  { tabuleiro = novoTabuleiro,
                    estadoPromocao = Nothing,
                    estadoAtual = GameOver,
                    mensagem = "Cheque Mate",
                    tempoFinalizacao = Just 0
                  }
              else
                let verificaXeque = verificarXeque novoTabuleiro (alternarCor corPeao)
                    novaMensagem =
                      if verificaXeque
                        then "Cheque!"
                        else if alternarCor corPeao == Branca then "Turno das brancas" else "Turno das pretas"
                 in estado
                      { tabuleiro = novoTabuleiro,
                        estadoPromocao = Nothing,
                        mensagem = novaMensagem,
                        corAtual = alternarCor corPeao
                      }
      return novoEstado
    Nothing -> return estado
tratarEventoJogo (EventKey (MouseButton LeftButton) Down _ mousePos) estado@(EstadoJogo {estadoPromocao = Nothing, posicaoSelecionada = Nothing, estadoAtual = _}) =
  case mouseParaPosicao mousePos of
    Just pos ->
      let (pecaChar, _) = pecaNaPosicao pos (tabuleiro estado)
          peca = if pecaChar /= ' ' then Just (charToPeca pecaChar) else Nothing
       in case peca of
            Just p ->
              if corPeca p == corAtual estado
                then return $ estado {posicaoSelecionada = Just pos}
                else return estado
            Nothing -> return estado
    Nothing -> return estado
tratarEventoJogo (EventKey (MouseButton LeftButton) Down _ mousePos) estado@(EstadoJogo {estadoPromocao = Nothing, posicaoSelecionada = Just origem, estadoAtual = estadoAtualJogo}) =
  case mouseParaPosicao mousePos of
    Just destino ->
      let (pecaDestinoChar, _) = pecaNaPosicao destino (tabuleiro estado)
          pecaCapturada = if pecaDestinoChar /= ' ' then Just (charToPeca pecaDestinoChar) else Nothing
          movimento = posicaoParaString origem destino
          novoTabuleiro = processarMovimento movimento (tabuleiro estado) (corAtual estado)
       in case novoTabuleiro of
            Just tabAtualizado ->
              if peaoPrecisaPromocao tabAtualizado destino
                then
                  return $
                    estado
                      { tabuleiro = tabAtualizado,
                        posicaoSelecionada = Nothing,
                        estadoPromocao = Just (destino, corAtual estado)
                      }
                else do
                  let (novasBrancas, novasPretas) = case pecaCapturada of
                        Just p ->
                          if corPeca p == Branca
                            then (p : capturadasBrancas estado, capturadasPretas estado)
                            else (capturadasBrancas estado, p : capturadasPretas estado)
                        Nothing -> (capturadasBrancas estado, capturadasPretas estado)
                      novoCor = alternarCor (corAtual estado)
                      verificaCheckMate = verificarXequeMate tabAtualizado novoCor
                      verificaCheck = verificarXeque tabAtualizado novoCor
                      novoEstadoBase =
                        estado
                          { tabuleiro = tabAtualizado,
                            corAtual = novoCor,
                            posicaoSelecionada = Nothing,
                            capturadasBrancas = novasBrancas,
                            capturadasPretas = novasPretas
                          }
                      novoEstado =
                        if verificaCheckMate
                          then
                            novoEstadoBase
                              { estadoAtual = GameOver,
                                mensagem = "Cheque Mate",
                                tempoFinalizacao = Just 0
                              }
                          else
                            let novaMensagem =
                                  if verificaCheck
                                    then "Cheque!"
                                    else if novoCor == Branca then "Turno das brancas" else "Turno das pretas"
                             in novoEstadoBase
                                  { mensagem = novaMensagem
                                  }
                  if estadoAtualJogo == UmJogador && not verificaCheckMate && novoCor == Preta
                    then do
                      aiMove <- getBestMove (tabuleiro novoEstado) Preta
                      case aiMove of
                        Just mv -> do
                          let movimentoAI = parseMove mv
                          case movimentoAI of
                            Just (origAI, destAI) -> do
                              let movimentoStr = posicaoParaString origAI destAI
                                  tabAI = processarMovimento movimentoStr (tabuleiro novoEstado) Preta
                              case tabAI of
                                Just tabNovaAI ->
                                  if peaoPrecisaPromocao tabNovaAI destAI
                                    then do
                                      let estadoComPromocaoAI =
                                            novoEstado
                                              { tabuleiro = tabNovaAI,
                                                estadoPromocao = Just (destAI, Preta),
                                                mensagem = "Promova o peão das pretas"
                                              }
                                      return estadoComPromocaoAI
                                    else do
                                      let verificaCheckMateAI = verificarXequeMate tabNovaAI (alternarCor Preta)
                                          verificaCheckAI = verificarXeque tabNovaAI (alternarCor Preta)
                                          estadoFinalAI =
                                            if verificaCheckMateAI
                                              then
                                                novoEstado
                                                  { tabuleiro = tabNovaAI,
                                                    estadoAtual = GameOver,
                                                    mensagem = "Cheque Mate",
                                                    tempoFinalizacao = Just 0
                                                  }
                                              else
                                                let novaMensagemAI =
                                                      if verificaCheckAI
                                                        then "Cheque!"
                                                        else "Turno das brancas"
                                                 in novoEstado
                                                      { tabuleiro = tabNovaAI,
                                                        corAtual = alternarCor Preta,
                                                        mensagem = novaMensagemAI
                                                      }
                                      return estadoFinalAI
                                Nothing -> return novoEstado {mensagem = "Movimento AI inválido"}
                            Nothing -> return novoEstado
                        Nothing -> return novoEstado {mensagem = "AI não conseguiu encontrar um movimento"}
                    else return novoEstado
            Nothing -> return $ estado {posicaoSelecionada = Nothing, mensagem = "Movimento inválido"}
    Nothing -> return $ estado {posicaoSelecionada = Nothing, mensagem = "Movimento inválido"}
tratarEventoJogo _ estado = return estado

parseMove :: String -> Maybe (Posicao, Posicao)
parseMove mv =
  if length mv < 4
    then Nothing
    else
      let orig = (colToIndex (mv !! 0), rowToIndex (mv !! 1))
          dest = (colToIndex (mv !! 2), rowToIndex (mv !! 3))
       in Just (orig, dest)

colToIndex :: Char -> Int
colToIndex c = fromEnum (toLower c) - fromEnum 'a'

rowToIndex :: Char -> Int
rowToIndex c = 8 - (fromEnum c - fromEnum '0')

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
pecaParaChar _ = ' '

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

peaoPrecisaPromocao :: Tabuleiro -> Posicao -> Bool
peaoPrecisaPromocao tab (x, y) =
  let pecaChar = (tab !! y) !! x
      peca = charToPeca pecaChar
   in case peca of
        Peao c -> (c == Branca && y == 0) || (c == Preta && y == 7)
        _ -> False

atualizarEstado :: Float -> EstadoJogo -> IO EstadoJogo
atualizarEstado delta estado =
  case estadoAtual estado of
    GameOver ->
      case tempoFinalizacao estado of
        Just t ->
          if t + delta >= 10
            then exitSuccess
            else return $ estado {tempoFinalizacao = Just (t + delta)}
        Nothing -> return $ estado {tempoFinalizacao = Just delta}
    _ -> return estado

mouseParaPosicao :: (Float, Float) -> Maybe Posicao
mouseParaPosicao (x, y) =
  let posX = floor ((x + 300) / 75)
      posY = 7 - floor ((y + 300) / 75)
   in if x >= -300 && x <= 300 && y >= -300 && y <= 300
        then Just (posX, posY)
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