{-
Ainda nao foi possivel configurar nenhum teste utilizando quickcheck
Implementamos estes testes basicos para testar o funcionamento
Alem disso, por algum motivo nao conseguimos trocar o nome do arquivo
Todas as vezes que tentamos trocar o nome do arquivo o stack run reclamava
-}

import ProcessarMovimento (processarMovimento)
import Tabuleiro (Cor (Branca), Tabuleiro, mostrarTabuleiro)

-- Funcao para configurar o tabuleiro para o roque pequeno
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

-- Funcao para configurar o tabuleiro para o roque grande
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

-- Funcao de teste para verificar o roque pequeno
testarRoquePequeno :: IO ()
testarRoquePequeno = do
  let tabuleiro = configurarTabuleiroParaRoquePequeno
  putStrLn "Tabuleiro antes do roque pequeno:"
  mostrarTabuleiro tabuleiro

  let movimento = "e1g1" -- Movimento de roque pequeno para as brancas
  let resultado = processarMovimento movimento tabuleiro Branca

  case resultado of
    Just novoTabuleiro -> do
      putStrLn "Tabuleiro apos o roque pequeno:"
      mostrarTabuleiro novoTabuleiro
    Nothing -> putStrLn "Movimento de roque pequeno invalido!"

-- Funcao de teste para verificar o roque grande
testarRoqueGrande :: IO ()
testarRoqueGrande = do
  let tabuleiro = configurarTabuleiroParaRoqueGrande
  putStrLn "Tabuleiro antes do roque grande:"
  mostrarTabuleiro tabuleiro

  let movimento = "e1c1" -- Movimento de roque grande para as brancas
  let resultado = processarMovimento movimento tabuleiro Branca

  case resultado of
    Just novoTabuleiro -> do
      putStrLn "Tabuleiro apos o roque grande:"
      mostrarTabuleiro novoTabuleiro
    Nothing -> putStrLn "Movimento de roque grande invalido!"

main :: IO ()
main = do
  testarRoquePequeno
  testarRoqueGrande