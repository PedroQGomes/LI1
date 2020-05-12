-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g101 where

import LI11819

-- * Funções não-recursivas.

-- | Um 'Vetor' é uma 'Posicao' em relação à origem.
type Vetor = Posicao
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>

-- ** Funções sobre vetores

-- *** Funções gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (a,b) (x,y) = (a+x,y+b) 

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (a,b) (x,y) = (a-x,b-y) 

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Int -> Vetor -> Vetor
multiplicaVetor x (a,b) = (x*a,x*b) 

-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distância à origem).
--
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
rodaVetor :: Vetor -> Vetor
rodaVetor (x,y) = (y,-x)

-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
--
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
inverteVetorH :: Vetor -> Vetor
inverteVetorH (x,y) = (x,-y)  

-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
--
-- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
inverteVetorV :: Vetor -> Vetor
inverteVetorV (x,y) = (-x,y) 

-- *** Funções do trabalho sobre 'Vetor'es.

-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor C = (-1,0)
direcaoParaVetor D = (0,1)
direcaoParaVetor B = (1,0)
direcaoParaVetor E = (0,-1)

-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido _ [] = False 
eIndiceListaValido 0 _ = True
eIndiceListaValido x (_:t) = eIndiceListaValido (x-1) t


-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz a = if null (head a) then (0,0) else (length a, length(head a))

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool 
ePosicaoMatrizValida _ [] = False
ePosicaoMatrizValida (x,y) (h:t) = eIndiceListaValido y h && (length (h:t) > x) && x >= 0 && y >= 0

-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -> Matriz a -> Bool
eBordaMatriz _ [] = False
eBordaMatriz (x,y) l@(h:_) = x == 0 || y == (length h - 1) || (y == 0) || x == (length l - 1)

-- *** Funções do trabalho sobre matrizes.

-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
--
-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -> Matriz Bool
tetrominoParaMatriz I = atualizaPosicaoMatriz (0,1) True (atualizaPosicaoMatriz (1,1) True (atualizaPosicaoMatriz (2,1) True (atualizaPosicaoMatriz (3,1) True (criaMatriz (4,4) False)) ) ) 
tetrominoParaMatriz J = atualizaPosicaoMatriz (2,0) True (atualizaPosicaoMatriz(0,1) True (atualizaPosicaoMatriz(1,1) True (atualizaPosicaoMatriz(2,1) True (criaMatriz(3,3) False))))
tetrominoParaMatriz L = atualizaPosicaoMatriz (2,2) True (atualizaPosicaoMatriz(0,1) True (atualizaPosicaoMatriz(1,1) True (atualizaPosicaoMatriz(2,1) True (criaMatriz(3,3) False))))
tetrominoParaMatriz O = criaMatriz (2,2) True
tetrominoParaMatriz S = atualizaPosicaoMatriz (0,1) True (atualizaPosicaoMatriz (0,2) True (atualizaPosicaoMatriz(1,1) True (atualizaPosicaoMatriz(1,0) True (criaMatriz (3,3) False))))
tetrominoParaMatriz Z = atualizaPosicaoMatriz (0,1) True (atualizaPosicaoMatriz (0,0) True (atualizaPosicaoMatriz(1,1) True (atualizaPosicaoMatriz(1,2) True (criaMatriz (3,3) False))))
tetrominoParaMatriz T = atualizaPosicaoMatriz (1,0) True (atualizaPosicaoMatriz (1,1) True (atualizaPosicaoMatriz (1,2) True (atualizaPosicaoMatriz (2,1) True (criaMatriz(3,3) False))))

-- * Funções recursivas.

-- ** Funções sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 l = head l
encontraIndiceLista x (_:t) = encontraIndiceLista (x-1) t

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista 0 a (_:t) = a:t
atualizaIndiceLista x a (h:t) = h:atualizaIndiceLista (x-1) a t

-- ** Funções sobre matrizes.

-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
--
-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>
rodaMatriz :: Matriz a -> Matriz a
rodaMatriz m = map reverse (transpose m)
 
-- | "transpose" faz a transposta de uma matriz 
transpose :: [[a]]->[[a]] 
transpose [] = []
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- | Inverte uma 'Matriz' na horizontal.
--
-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
inverteMatrizH :: Matriz a -> Matriz a
inverteMatrizH = map reverse

-- | Inverte uma 'Matriz' na vertical.
--
-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
inverteMatrizV :: Matriz a -> Matriz a
inverteMatrizV [] = []
inverteMatrizV [a] = [a]
inverteMatrizV (h:t) = last t:inverteMatrizV(h:init t)

-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -> a -> Matriz a
criaMatriz (x,y) a = replicate x (replicate y a)

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz (0,0) (h:_) = head h
encontraPosicaoMatriz (0,y) (h:_) = encontraPosicaoLista y h
encontraPosicaoMatriz (x,y) (_:t) = encontraPosicaoMatriz (x-1,y) t

-- |"encontraPosicaoLista x a" devolve o elemento numa dada posição "Int" de uma lista "[a]""
encontraPosicaoLista :: Int -> [a] -> a
encontraPosicaoLista 0 l = head l
encontraPosicaoLista x (_:t) = encontraPosicaoLista (x-1) t  
 
-- | Modifica um elemento numa dada 'Posicao'
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz _ _ [] = []
atualizaPosicaoMatriz (0,y) e (h:t) = atualizaIndiceLista y e h : t
atualizaPosicaoMatriz (x,y) e (h:t) = h : atualizaPosicaoMatriz (x - 1, y) e t

