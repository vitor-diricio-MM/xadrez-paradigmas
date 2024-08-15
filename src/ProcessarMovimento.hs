module ProcessarMovimento (
    processarMovimento,
    moverPeca
) where

import Check (verificarXeque)
import Tabuleiro (Cor (..), Posicao, Tabuleiro)
import Utils (colunaParaIndice, corPeca, linhaParaIndice, pecaNaPosicao')
import ValidacaoMovimento (movimentoValido, roqueValido)


-- Função para Processar Movimento
processarMovimento :: String -> Tabuleiro -> Cor -> Maybe Tabuleiro
processarMovimento [c1, r1, c2, r2] tab corAtual =
    let inicio = (colunaParaIndice c1, linhaParaIndice r1)
        fim = (colunaParaIndice c2, linhaParaIndice r2)
        peca = pecaNaPosicao' inicio tab
     in case peca of
            Just p ->
                if corPeca p == corAtual && movimentoValido tab p inicio fim
                    then
                        let novoTabuleiro = if roqueValido tab p inicio fim then executarRoque inicio fim tab else moverPeca inicio fim tab
                         in if not (verificarXeque novoTabuleiro corAtual)
                                then Just novoTabuleiro
                                else Nothing
                    else Nothing
            Nothing -> Nothing
processarMovimento _ _ _ = Nothing


moverPeca :: Posicao -> Posicao -> Tabuleiro -> Tabuleiro
moverPeca (x1, y1) (x2, y2) tab = 
    let
        -- Remover a peça da posição inicial
        tabSemPecaInicial = atualizarTabuleiro tab (x1, y1) ' '
        
        -- Colocar a peça na posição final
        peca = tab !! y1 !! x1
        tabFinal = atualizarTabuleiro tabSemPecaInicial (x2, y2) peca
    in
        tabFinal

-- Função auxiliar para atualizar o tabuleiro em uma posição específica
atualizarTabuleiro :: Tabuleiro -> Posicao -> Char -> Tabuleiro
atualizarTabuleiro tab (x, y) peca =
    let
        linha = tab !! y
        linhaAtualizada = take x linha ++ [peca] ++ drop (x + 1) linha
    in
        take y tab ++ [linhaAtualizada] ++ drop (y + 1) tab

executarRoque :: Posicao -> Posicao -> Tabuleiro -> Tabuleiro
executarRoque (x1, y1) (x2, y2) tab
    | x2 == 6 = 
        -- Roque pequeno: move a Torre de (7, y1) para (5, y1) e o Rei para (6, y1)
        let tabComRei = moverPeca (x1, y1) (x2, y2) tab
            tabFinal = moverPeca (7, y1) (5, y1) tabComRei
        in tabFinal
    | x2 == 2 = 
        -- Roque grande: move a Torre de (0, y1) para (3, y1) e o Rei para (2, y1)
        let tabComRei = moverPeca (x1, y1) (x2, y2) tab
            tabFinal = moverPeca (0, y1) (3, y1) tabComRei
        in tabFinal
    | otherwise = tab