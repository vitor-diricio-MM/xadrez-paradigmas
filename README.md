# Projeto Xadrez | Programacao funcional ![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)

<p align="center">
<img src="https://i.ibb.co/0DkWq1G/Ufabc-logo.png" width="100" height="100" alt="Logo UFABC">
</p>

Participantes:

- Vitor Bobig Diricio | 11201811376
- Joao Pedro Machado | 11201720180
- Thiago Schwartz Machado | 11202130845

Este projeto é uma evolução do jogo de xadrez em Haskell, agora com uma interface gráfica criada usando a biblioteca Gloss. O jogo permite partidas entre dois jogadores humanos ou contra uma inteligência artificial básica que performa movimentos bastante simplórios.

## Funcionalidades

- Gerenciamento de cliques: Implementado tanto para interação com peças quanto com o menu.
- Promoção de peões: Adicionada nesta entrega (através de um pop up que surge), completando as funcionalidades do jogo de xadrez. Na promoção de peões, o usuário escolhe a peça de promoção tanto para si quanto para a IA. Uma melhoria futura seria implementar uma lógica para que a IA escolha automaticamente a peça mais adequada para a promoção
- Finalização do jogo após xeque mate: Exibe "Xeque Mate" por 10 segundos antes de fechar. Uma melhoria seria retornar ao menu em vez de fechar.
- Histórico de peças eliminadas: Peças capturadas são exibidas ao redor do tabuleiro.

## Dificuldades, destaques e surpresas

- Interação drag and drop: Inicialmente queríamos que o movimento das peças fosse feito por drag and drop, mas tivemos extrema dificuldade e optamos pela implementação com 2 cliques
- Gerenciamento de estados: Manter o estado do jogo atualizado de maneira otimizada foi confuso e complicado. Um destaque para a função tratarEventoJogo em Gui.hs, que poderia ser simplificada com Monads State, mas não conseguimos implementar a tempo.
- Cliques no menu: Tivemos muitos problemas no design dos botões e na sua área de clique.
- IA:A complexidade em desenvolver uma AI robusta resultou em movimentos simplórios, destacando a necessidade de aprimoramento futuro.

## Instalacao e usabilidade

Para rodar o codigo, basta clonar o repositorio e rodar

```bash
stack run
```

## Como jogar

1. Selecione o estilo de jogo ("um jogador" ou "dois jogadores").

2. Peças brancas começam

3. Clique na peça que deseja movimentar e depois na casa de destino
