import ProcessarMovimento (processarMovimento)
import Tabuleiro

-- Função para configurar o tabuleiro para o roque pequeno
configurarTabuleiroParaRoquePequeno :: Tabuleiro
configurarTabuleiroParaRoquePequeno =
  [ "rnbqkbnr",
    "pppppppp",
    "        ",
    "        ",
    "        ",
    "        ",
    "PPPPPPPP",
    "R   K  R"
  ]

-- Função para configurar o tabuleiro para o roque grande
configurarTabuleiroParaRoqueGrande :: Tabuleiro
configurarTabuleiroParaRoqueGrande =
  [ "rnbqkbnr",
    "pppppppp",
    "        ",
    "        ",
    "        ",
    "        ",
    "PPPPPPPP",
    "R   K  R"
  ]

-- Função de teste para verificar o roque pequeno
testarRoquePequeno :: IO ()
testarRoquePequeno = do
  let tabuleiro = configurarTabuleiroParaRoquePequeno
  putStrLn "Tabuleiro antes do roque pequeno:"
  mostrarTabuleiro tabuleiro

  let movimento = "e1g1" -- Movimento de roque pequeno para as brancas
  let resultado = processarMovimento movimento tabuleiro Branca

  case resultado of
    Just novoTabuleiro -> do
      putStrLn "Tabuleiro após o roque pequeno:"
      mostrarTabuleiro novoTabuleiro
    Nothing -> putStrLn "Movimento de roque pequeno inválido!"

-- Função de teste para verificar o roque grande
testarRoqueGrande :: IO ()
testarRoqueGrande = do
  let tabuleiro = configurarTabuleiroParaRoqueGrande
  putStrLn "Tabuleiro antes do roque grande:"
  mostrarTabuleiro tabuleiro

  let movimento = "e1c1" -- Movimento de roque grande para as brancas
  let resultado = processarMovimento movimento tabuleiro Branca

  case resultado of
    Just novoTabuleiro -> do
      putStrLn "Tabuleiro após o roque grande:"
      mostrarTabuleiro novoTabuleiro
    Nothing -> putStrLn "Movimento de roque grande inválido!"

main :: IO ()
main = do
  testarRoquePequeno
  testarRoqueGrande