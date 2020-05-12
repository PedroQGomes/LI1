-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g101 where
import LI11819
import Tarefa0_2018li1g101

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[Move D,Move B,Move B,Move D,Move D,Move B,Move B,Move D,Move B,Move D,Move B,Move D,Move D,Move B,Move B,Move D,Move D,Move B,Move D,Move B,Move D,Move B,Move D,Move D,Move B,Move D,Move B,Move D,Move D,Move B,Move D,Move B,Move D,Move D,Move E,Move E,Move C,Move D,Move D,Move D,Move D,Move E,Move E,Move E,Move E,Move C,Move C,Move E,Move E,Move C,Move E,Move E,Move E,MudaParede,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move C,Move C,Move B,Move B,Move C,Move C,Move E,Move B,Move B,Move B,Move C,Move C,Desenha,Move C,Desenha,Move C,Move C,Desenha,Move C,Desenha,Move C,Desenha,Move C,Desenha,Move C,Desenha,Move C,Desenha,Move C,Move C,Desenha,Move C,Desenha,Move C,Desenha,Move C,Desenha,Move C,Desenha,Desenha,Move C,Roda,Move C,Move D,Desenha,Move D,Desenha,Move D,Desenha,Move D,Move D,Desenha,Move D,Desenha,Move D,Desenha,Move D,Desenha,Move D,Desenha,Move D,Desenha,Move D,Desenha,Move D,Desenha,Move D,Desenha,Desenha,Move D,Desenha,Move D,Desenha,Move D,Desenha,Desenha,Move D,Roda,Move B,Move D,Desenha,Move B,Desenha,Move B,Desenha,Move B,Move B,Desenha,Move B,Desenha,Move B,Desenha,Move B,Desenha,Move B,Desenha,Move B,Move B,Desenha,Move B,Desenha,Move B,Desenha,Desenha,Move B,Desenha,Move B,Desenha,Move B,Desenha,Desenha,Roda,Move E,Move E,Move B,Desenha,Move E,Move E,Desenha,Move E,Desenha,Move E,Desenha,Move E,Desenha,Move E,Desenha,Move E,Desenha,Move E,Desenha,Move E,Desenha,Move E,Desenha,Move E,Desenha,Desenha,Move E,Move E,Desenha,Move E,Desenha,Move C,Move C,Move C,Roda,Move D,Move D,Move E,Move C,Desenha,Move C,Desenha,Move C,Desenha,Move C,Move C,Desenha,Move C,Desenha,Move C,Desenha,Move C,Desenha,Move C,Move C,Desenha,Move D,Move C,Roda,Desenha,Move D,Desenha,Move D,Desenha,Move D,Desenha,Move D,Move D,Desenha,Move D,Desenha,Move D,Desenha,Move D,Desenha,Move D,Desenha,Move D,Desenha,Desenha,Move B,Desenha,Move D,Roda,Desenha,Move B,Desenha,Move B,Move B,Desenha,Move B,Desenha,Move B,Desenha,Move B,Desenha,Move B,Move B,Desenha,Move B,Desenha,Roda,Move B,Move E,Move E,Move E,Desenha,Desenha,Move E,Desenha,Move E,Desenha,Move E,Move E,Desenha,Move E,Desenha,Move E,Desenha,Desenha,Move E,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Desenha,Move C,Move C,Move C,Move C,MudaParede,Move C,Move C,Move D,Move D,Move D,Move D,Move C,Move D,Move D,Desenha,Move B,Desenha,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Move E,Desenha,Move C,Desenha,Desenha,Move D,Move D,Move D,Move D,Roda,Move C,Move C,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move D,Move C,Move C,Move C,Move C,Move C,Move E,Move C,Desenha,Move E,Desenha,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Move B,Desenha,Move D,Desenha,Move C]]




-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
-- ^ A 'Instrucao' a aplicar.
-- ^ O 'Editor' anterior.
-- ^ O 'Editor' resultante após aplicar a 'Instrucao'.
instrucao :: Instrucao -> Editor -> Editor  
instrucao (Move D) edit = edit { posicaoEditor = somaVetores (direcaoParaVetor D) (posicaoEditor edit)  }
instrucao (Move C) edit = edit { posicaoEditor = somaVetores (direcaoParaVetor C) (posicaoEditor edit)  }
instrucao (Move B) edit = edit { posicaoEditor = somaVetores (direcaoParaVetor B) (posicaoEditor edit)  }
instrucao (Move E) edit = edit { posicaoEditor = somaVetores (direcaoParaVetor E) (posicaoEditor edit) }
instrucao MudaTetromino edit =  edit{ tetrominoEditor = trocaTetromino (tetrominoEditor edit)}
instrucao Roda edit = edit { direcaoEditor = trocaDirecao (direcaoEditor edit) }
instrucao MudaParede edit = edit {paredeEditor = trocaParede (paredeEditor edit) }
instrucao Desenha edit = desenhaTetromino edit  


-- | Desenha tetromino

desenhaTetromino :: Editor -> Editor
desenhaTetromino edit = edit { mapaEditor = atualizaMapa (rodaMatrizTetromino(tetrominoParaMatriz (tetrominoEditor edit)) (direcaoEditor edit)) (paredeEditor edit,0) (posicaoEditor edit) (mapaEditor edit) }


-- | funcao "atualizaMapa" : recebe a matriz bool a implemententar, o tipo de parede, um contador para as linhas, a posiçao do editor e o mapa a modificar 
atualizaMapa :: Matriz Bool -> (Parede , Int) -> Posicao -> Mapa -> Mapa
atualizaMapa [] _ _ l = l
atualizaMapa _ _  _ [] = []
atualizaMapa (x:xs) (par,a) (l,c) (h:t) | a >= l = atualizaLista2 x (par,0) c h : atualizaMapa xs (par ,a + 1) (l, c) t
                                      | otherwise = h : atualizaMapa (x : xs) (par ,a + 1) (l, c) t



-- | atualizaLista2 recebe uma lista de bools a implementar , o tipo de parede , um contador para as colunas, a posicao na linha que deve ser implementado e a lista de pecas a modificar
atualizaLista2 :: [Bool] -> (Parede,Int) -> Int-> [Peca] -> [Peca]
atualizaLista2 [] _ _ q = q
atualizaLista2 _ _ _ [] = []
atualizaLista2 (h:t) (par,cont) l (a:b) | cont < l = a:atualizaLista2 (h:t) (par,cont+1) l b 
                                     | not h = a:atualizaLista2 t (par,cont+1) l b
                                     | otherwise = Bloco par: atualizaLista2 t (par,cont+1) l b  
                                          

-- | Roda a Matriz Bool de acordo com a direcao

rodaMatrizTetromino :: Matriz Bool -> Direcao -> Matriz Bool
rodaMatrizTetromino l C = l
rodaMatrizTetromino l D = rodaMatriz l
rodaMatrizTetromino l B = rodaMatriz $ rodaMatriz  l
rodaMatrizTetromino l E = rodaMatriz $ rodaMatriz $ rodaMatriz l

-- | Troca a direção 

trocaDirecao :: Direcao -> Direcao
trocaDirecao C = D
trocaDirecao D = B
trocaDirecao B = E
trocaDirecao E = C

-- | Troca o Tetronimo 

trocaTetromino :: Tetromino -> Tetromino
trocaTetromino I = J
trocaTetromino J = L
trocaTetromino L = O
trocaTetromino O = S
trocaTetromino S = T
trocaTetromino T = Z
trocaTetromino Z = I

-- | Troca o tipo de Parede

trocaParede :: Parede -> Parede
trocaParede Indestrutivel = Destrutivel
trocaParede Destrutivel = Indestrutivel

-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
-- ^ As 'Instrucoes' a aplicar
-- ^ O 'Editor' anterior.
-- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes :: Instrucoes -> Editor -> Editor     
instrucoes t edit = foldr instrucao edit (reverse t) 

-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
-- ^ O 'Mapa' resultante com a 'Dimensao' dada
-- ^ A 'Dimensao' do 'Mapa' a criar.
mapaInicial :: Dimensao -> Mapa     
mapaInicial (x,y) = bordaIndestrutivel(criaMatriz (x,y) Vazia) 0 (x-1,y-1)

-- | "bordaIndestrutivel" aualiza a matriz vazia com blocos indestrutiveis nas bordas
bordaIndestrutivel ::  Matriz Peca -> Int -> Posicao ->  Mapa
bordaIndestrutivel [] _ _ = []
bordaIndestrutivel (h:t) a (x,y) | a == 0 || a == x = listaIndestrutivel h :bordaIndestrutivel t (a+1) (x,y) 
                                 | otherwise = ([Bloco Indestrutivel] ++ init (tail h) ++ [Bloco Indestrutivel]):bordaIndestrutivel t (a+1) (x,y) 
-- | "listaIndestrutivel" recebe uma lista de peças e retorna uma lista de blocos indestrutiveis
listaIndestrutivel :: [Peca] -> [Peca]
listaIndestrutivel [] = []
listaIndestrutivel [_] = [Bloco Indestrutivel]
listaIndestrutivel (_:t) = Bloco Indestrutivel : listaIndestrutivel t

-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
-- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
-- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial :: Instrucoes -> Editor      
editorInicial is = Editor { tetrominoEditor = I 
                            , paredeEditor = Indestrutivel
                            , direcaoEditor = C 
                            , posicaoEditor = posicaoInicial is 
                            , mapaEditor = mapaInicial (dimensaoInicial is) }  



-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
 -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
-- ^ O 'Mapa' resultante.
constroi :: Instrucoes -> Mapa       
constroi is = mapaEditor (instrucoes is (editorInicial is))





