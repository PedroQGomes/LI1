-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g101 where
import Tarefa1_2018li1g101
import Tarefa0_2018li1g101
import LI11819

-- * Teste

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
--testesT2 :: [(Int,Jogada,Estado)]
--testesT2 = [(0, Dispara Canhao,jogada(0,Movimenta C,jogada(0,Movimenta D,jogada(0,Movimenta E,jogada(0,Movimenta B,jogada (0,Dispara Laser,--jogada (0,Dispara Choque,estadoTeste)))))))]
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0, Dispara Canhao,estadoTeste),(0, Dispara Laser,estadoTeste),(0, Dispara Choque,estadoTeste),(0, Movimenta C,estadoTeste),(0,Movimenta B,estadoTeste),(0, Movimenta D,estadoTeste),(0, Movimenta E,estadoTeste)]



-- * Estados para Testar

-- | Estado Teste 1
estadoTeste :: Estado
estadoTeste = Estado {mapaEstado = mapaInicial(8,8) , jogadoresEstado = [Jogador {posicaoJogador = (5,5), direcaoJogador = C, vidasJogador = 5, lasersJogador = 0, choquesJogador = 1},Jogador {posicaoJogador = (1,1), direcaoJogador = C, vidasJogador = 5, lasersJogador = 0, choquesJogador = 1},Jogador {posicaoJogador = (3,3), direcaoJogador = C, vidasJogador = 5, lasersJogador = 0, choquesJogador = 1},Jogador {posicaoJogador = (1,5), direcaoJogador = C, vidasJogador = 5, lasersJogador = 0, choquesJogador = 1}], disparosEstado =  [] }


-- | Estado Teste 2
estadoTeste2 :: Estado
estadoTeste2 = Estado (mapaInicial (16,16)) [Jogador (5,5) C 5 5 5 , Jogador (2,8) C 4 0 4 , Jogador (10,10) B 4 1 4 , Jogador (3,4) B 0 2 1] [DisparoCanhao 0 (9,10) B,DisparoCanhao 0 (1,8) B, DisparoCanhao 0 (4,4) B,DisparoLaser 2 (1,1) B]

-- | Estado Teste 3
estadoTeste3 :: Estado
estadoTeste3 = Estado {-mapaT6-}(mapaInicial (20,20)) [Jogador (1,4) D 5 5 5] [DisparoCanhao 0 (1,5) D]

-- | Mapa para testar 4
mapaT4 :: Mapa
mapaT4 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]

-- | Mapa para testar 5
mapaT5 :: Mapa 
mapaT5 = atualizaPosicaoMatriz (1,1) (Bloco Destrutivel) (mapaInicial(20,20))

-- | Mapa para testar 6
mapaT6 :: Mapa  
mapaT6 = atualizaPosicaoMatriz (1,2) (Bloco Indestrutivel) mapaT5

-- | Mapa para testar 7
mapaT7 :: Mapa  
mapaT7 = atualizaPosicaoMatriz (2,1) (Bloco Destrutivel) mapaT6

-- * Funções principais da Tarefa 2.
--
-- | Efetua uma jogada.
jogada :: Int -> Jogada -> Estado -> Estado
jogada njog (Movimenta dir) estado =  if vidasJogador jogador >0 then estado { jogadoresEstado = atualizaIndiceLista njog (moverJogador jogador estado dir) (jogadoresEstado estado) } else estado
                                    where jogador = encontraPosicaoLista njog (jogadoresEstado estado)
jogada njog (Dispara arma) estado = if podeDisparar arma jogador && vidasJogador jogador >0 then  estado {disparosEstado =  disparosEstado estado ++ [criaDisparo arma njog estado],jogadoresEstado = atualizaIndiceLista njog (decrementaDisparo arma jogador) (jogadoresEstado estado)} else estado
                                where jogador = encontraPosicaoLista njog (jogadoresEstado estado)

-- | verifica se o jogador tem disparos para poder disparar uma certa arma
podeDisparar :: Arma -> Jogador -> Bool
podeDisparar Canhao jogador = vidasJogador jogador > 0
podeDisparar Laser jogador = lasersJogador jogador > 0 && vidasJogador jogador > 0
podeDisparar Choque jogador = choquesJogador jogador > 0 && vidasJogador jogador > 0


-- | decrementa o numero de disparos depois de efetuar um 
decrementaDisparo :: Arma -> Jogador -> Jogador
decrementaDisparo Laser jogador = jogador { lasersJogador = lasersJogador jogador -1 }
decrementaDisparo Choque jogador = jogador{ choquesJogador = choquesJogador jogador -1 }
decrementaDisparo _ players = players




-- | verifica se a direcao do jogador é igual a direcao do moviemento
direcaoIgualMovimento :: Direcao -> Jogador -> Bool
direcaoIgualMovimento d jogador = direcaoJogador jogador == d

-- | "criaDisparo" que recebe uma "Arma" o numero do jogador que dispara  "Int" e um "Estado" e retorna o "Disparo" efetuado
criaDisparo :: Arma -> Int -> Estado  -> Disparo
criaDisparo Canhao njog estado  = DisparoCanhao { jogadorDisparo = njog , posicaoDisparo = somaVetores (direcaoParaVetor(direcaoJogador jogador)) (posicaoJogador jogador) , direcaoDisparo = direcaoJogador jogador }
                        where jogador = encontraPosicaoLista njog (jogadoresEstado estado)
criaDisparo Laser njog estado  = DisparoLaser { jogadorDisparo = njog , posicaoDisparo = somaVetores (direcaoParaVetor(direcaoJogador jogador)) (posicaoJogador jogador) , direcaoDisparo = direcaoJogador jogador }
                        where jogador = encontraPosicaoLista njog (jogadoresEstado estado)
criaDisparo Choque njog _  = DisparoChoque { jogadorDisparo = njog , tempoDisparo = 30 }

-- | "moverJogador" move uma "Jogador" numa dada "Direcao" e retorna o "Jogador" movido com essa "DireçaomoverJogador :: Jogador -> Direcao -> Jogador
moverJogador :: Jogador ->  Estado -> Direcao -> Jogador
moverJogador jogador est dir = jogador { posicaoJogador = if direcaoIgualMovimento dir jogador && podeMover jogador est dir  then somaVetores(direcaoParaVetor dir) (posicaoJogador jogador) else posicaoJogador jogador , direcaoJogador = dir}

-- | verifica se um jogador pode mover numa determinada direcao,ou seja se essa posicao está vazio
podeMover :: Jogador -> Estado -> Direcao -> Bool
podeMover jog est dir = verificaMoverValido (somaVetores (direcaoParaVetor dir) (posicaoJogador jog)) jog est && not (verificaChoque jog est  )

-- | Valida se o jogador não está a ser afetado pelo choque para ver se é possivel mover ou não
verificaChoque :: Jogador -> Estado -> Bool
verificaChoque jog est = verificaChoquesOcc jog jogEmChoque est
                        where jogEmChoque = jogadorEmChoque est

-- | Verifica se o Jogador é afetado pelo Choque                         
verificaChoquesOcc :: Jogador -> [Jogador] -> Estado -> Bool 
verificaChoquesOcc _ [] _ = False
verificaChoquesOcc jog (h:t) est = if h == jog then verificaChoquesOcc jog t est else validaChoque tankPos (posicaoJogador h) || verificaChoquesOcc jog t est
                                    where (x,y) = posicaoJogador jog
                                          tankPos = [(x+1,y),(x+1,y+1),(x,y+1),(x,y)]

-- | Recebe a Posicao do Jogador e a posição do Jogador em Choque e vê se está em choque
validaChoque :: [Posicao] -> Posicao -> Bool
validaChoque [] _ = False
validaChoque ((x,y):t) (cx,cy) = x <= cx+3 && x >= cx-2 && y >= cy-2 && y<= cy+3  || validaChoque t (cx,cy)

-- | Atraves do estado da retorno da lista de jogadores com Disparos Choque ativos
jogadorEmChoque :: Estado -> [Jogador]
jogadorEmChoque estado = constroiListaDeJog(disparosParaIndJog (disparosEstado estado)) estado

-- | Atraves da lista de indices de jogadores com Disparos Choque ativos cria uma lista de Jogadores
constroiListaDeJog :: [Int] -> Estado -> [Jogador]
constroiListaDeJog [] _ = []
constroiListaDeJog (h:t) est = jogador:constroiListaDeJog t est
                                where jogador = encontraPosicaoLista h (jogadoresEstado est)

-- | Constroi a lista de indices de jogadores com Disparos Choque ativos                                 
disparosParaIndJog ::  [Disparo] -> [Int]
disparosParaIndJog [] = []
disparosParaIndJog (h@DisparoChoque{jogadorDisparo = _ , tempoDisparo = _}:t) = jogadorDisparo h : disparosParaIndJog t  
disparosParaIndJog (_:t) = disparosParaIndJog t

-- | Valida se a posicao é valida para um jogador se mover para la
verificaMoverValido :: Posicao -> Jogador -> Estado -> Bool
verificaMoverValido (px,py) jog estado = encontraPosicaoMatriz (px,py) m == Vazia && encontraPosicaoMatriz (px+1,py) m == Vazia && encontraPosicaoMatriz (px+1,py+1) m == Vazia && encontraPosicaoMatriz (px,py+1) m == Vazia
                                where m =  mapaComTanques (jogadoresEstado estado) jog  (mapaEstado estado)

-- | Substiti os tanks por blocos indestrutiveis no mapa
mapaComTanques ::  [Jogador] -> Jogador -> Mapa -> Mapa
mapaComTanques [] _ m = m
mapaComTanques (h:t) jog m = if h == jog || vidasJogador h <= 0  then mapaComTanques t jog m else  mapaComTanques t jog (meteBlocoIndestrutivelNosTanques (posicaoJogador h) m)

-- | Mete blocos indestrutiveis no mapa no lugar dos tanques
meteBlocoIndestrutivelNosTanques ::  Posicao -> Mapa -> Mapa
meteBlocoIndestrutivelNosTanques (px,py)  m = atualizaPosicaoMatriz (px,py) (Bloco Indestrutivel) $ atualizaPosicaoMatriz (px+1,py) (Bloco Indestrutivel) $ atualizaPosicaoMatriz (px+1,py+1) (Bloco Indestrutivel)   $ atualizaPosicaoMatriz (px,py+1) (Bloco Indestrutivel) m                      