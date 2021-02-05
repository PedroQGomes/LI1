# MIEI - 1ºAno - Laboratórios de Informática 1

O trabalho pratico proposto consiste num jogo de tanks em 2D usando a linguagem Haskell. As principais tarefas eram as seguintes:

* Criar duas funções que comprimissem/descomprimissem o estado do jogo.
* Desenvolver uma interface gráfica para o jogo.
* Criar um bot que jogasse sozinho contra adversários.

## Dependencias

Como é usada a linguagem Haskell é necessario um compilador
```
sudo apt install ghc
```

Para a parte gráfica é usada a biblioteca Gloss. Para instalar o gloss é necessário a ferramenta cabal

```
cabal update
```

```
cabal install gloss
```

```
cabal install split
```


## Compilação e Testes

Para compilar e gerar o executavel é usado o seguinte comando

```
ghc -main-is Tarefa5_2018li1g101 Tarefa5_2018li1g101.hs
```

Para testar o jogo basta correr o executável Tarefa5_2018li1g101.exe gerado anteriormente.