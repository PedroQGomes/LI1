-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2018li1g101 where
import Tarefa4_2018li1g101
import Tarefa0_2018li1g101
import Tarefa2_2018li1g101
import LI11819
import Data.Maybe

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n e = moveOBot n e $ destroiParedeParaPassar n e $ atacaOsJogadores n e $ usaChoque n e $ defendeDosTiros Nothing n e
       
 
-- | Usa o choque se o jogador estiver suficientemente proximo (alcance de 3)      
usaChoque :: Int -> Estado -> Maybe Jogada -> Maybe Jogada
usaChoque _ _ (Just j) = Just j
usaChoque n e Nothing = if choquesJogador bot > 0 && not (botIsInChoque n (disparosEstado e)) && isJust nearbyJog && distanciaEntrePontos (posicaoJogador bot) (posicaoJogador $ fromJust nearbyJog) <= 3 then Just $ Dispara Choque else Nothing 
                  where bot = encontraIndiceLista n (jogadoresEstado e)
                        nearbyJog = nearbyPlayer bot e

-- | Verifica se o bot ja esta em choque
botIsInChoque :: Int -> [Disparo] -> Bool
botIsInChoque _ [] = False
botIsInChoque n (h@DisparoChoque{jogadorDisparo = jD, tempoDisparo = _}:t) = jD == n || botIsInChoque n t
botIsInChoque n (h:t) = botIsInChoque n t

-- | Destroi paredes para ganhar pontos e para desobstruir caminho
destroiParedeParaPassar :: Int -> Estado -> Maybe Jogada -> Maybe Jogada
destroiParedeParaPassar _ _ (Just j) = Just j
destroiParedeParaPassar n e Nothing = if isJust paredeIsClose &&  not (disparouRecentemente n e  (disparosEstado e)) then  (if dirParede == direcaoJogador bot then Just $ Dispara Canhao else Just $ Movimenta dirParede ) else Nothing
                                    where paredeIsClose = verificaParedePorPerto e [(x,y-1),(x+1,y-1),(x-1,y),(x-1,y+1),(x,y+2),(x+1,y+2),(x+2,y),(x+2,y+1)]
                                          bot = encontraIndiceLista n $ jogadoresEstado e
                                          (x,y) = posicaoJogador bot
                                          dirParede = calculaDirecaoParaParede (x,y) $ fromJust paredeIsClose

-- | Funcao que calcula a direcao que o bot tem de estar virado para destruir a parede                                          
calculaDirecaoParaParede :: Posicao -> Posicao -> Direcao
calculaDirecaoParaParede p posParede  | getDir C = C
                                      | getDir D = D 
                                      | getDir B = B
                                      | getDir E = E 
                                      | otherwise = C
                                      where getDir dir = (fst $ getPosDisp dir (pos dir)) == posParede || (snd $ getPosDisp dir (pos dir)) == posParede
                                            pos a = somaVetores p (direcaoParaVetor a)

-- | Verifica se uma parede destrutivel ta por perto                                          
verificaParedePorPerto :: Estado -> [Posicao] -> Maybe Posicao
verificaParedePorPerto _ [] = Nothing
verificaParedePorPerto e (h:t) = if peca h == Bloco Destrutivel then Just h else verificaParedePorPerto e t   
                        where peca pos = encontraPosicaoMatriz pos $ mapaEstado e 

-- | Verifica se acerta numa parede Destrutivel
vaiAcertarNaParedeDestrutivel ::  Int -> Estado -> Bool
vaiAcertarNaParedeDestrutivel n e = expandeDisparoAteBaterEmParede supostoDisparo e 
                              where bot = encontraIndiceLista n $ jogadoresEstado e     
                                    supostoDisparo = DisparoCanhao n (somaVetores (posicaoJogador bot) (direcaoParaVetor $ direcaoJogador bot)) (direcaoJogador bot)

-- | Verifica se o disparo bate na parede Destrutivel                                    
expandeDisparoAteBaterEmParede :: Disparo -> Estado -> Bool 
expandeDisparoAteBaterEmParede d e = if dest then True else if indst then False else expandeDisparoAteBaterEmParede d{posicaoDisparo = somaVetores (posicaoDisparo d) (direcaoParaVetor $ direcaoDisparo d)} e 
                               where (pos,pos1) = getPosDisp (direcaoDisparo d) (posicaoDisparo d)
                                     peca x = encontraPosicaoMatriz x (mapaEstado e)
                                     indst =  peca pos == Bloco Indestrutivel || peca pos1 == Bloco Indestrutivel
                                     dest = peca pos == Bloco Destrutivel || peca pos1 == Bloco Destrutivel 
                                     
                                     
-- | Movimenta o bot para o pe do 'Jogador' mais proximo 
moveOBot :: Int -> Estado -> Maybe Jogada -> Maybe Jogada
moveOBot _ _ (Just j) = Just j
moveOBot n e Nothing = if isJust nearbyJog then (if distanciaEntrePontos (posicaoJogador bot) (posicaoJogador $ fromJust nearbyJog) <=2 then rodaBotParaJogador bot e (posicaoJogador $ fromJust nearbyJog) else moveOBotParaPosicaoJogador n e $ posicaoJogador $ fromJust nearbyJog )  else Nothing
                  where bot = encontraIndiceLista n (jogadoresEstado e)
                        nearbyJog = nearbyPlayer bot e

-- | Roda o bot quando esta muito perto do jogador mais proximo de forma a poder disparar                        
rodaBotParaJogador :: Jogador -> Estado -> Posicao -> Maybe Jogada
rodaBotParaJogador jog e (x1,y1) | y > y1  = Just $ Movimenta E
                                 | y < y1  = Just $ Movimenta D
                                 | x < x1  = Just $ Movimenta B
                                 | x > x1  = Just $ Movimenta C
                                 | otherwise = Nothing
                              where (x,y) = posicaoJogador jog 

-- | Move o bot para ao pe de um 'Jogador' proximo                                    
moveOBotParaPosicaoJogador :: Int -> Estado -> Posicao -> Maybe Jogada
moveOBotParaPosicaoJogador n e pos1   = if  veSePodeMover jog dir  e && veSeNaoLevaTiro n e dir then Just $ Movimenta dir else Nothing
                                    where pos = posicaoJogador jog
                                          jog = encontraIndiceLista n $ jogadoresEstado e
                                          dir = aproximaJogador n e pos pos1
    
-- | Funcao que retorna a 'Direcao' para o bot se aproximar do 'Jogador' mais proximo                                           
aproximaJogador :: Int -> Estado -> Posicao -> Posicao -> Direcao
aproximaJogador n e (x,y) (x1,y1) | x > x1 = C
                                  | y > y1 = E
                                  | y < y1 = D
                                  | x < x1 = B
                                  | isJust tentaMover = fromJust tentaMover
                                  | otherwise = if isJust nearbyJog then rodaBotParaDirecaoJogador bot e (posicaoJogador $ fromJust nearbyJog) else B
                              where tentaMover = movimentaParaAlgumLado n e 
                                    bot = encontraIndiceLista n $ jogadoresEstado e
                                    nearbyJog = nearbyPlayer bot e
                                    

-- | Retorna a direcao do jogador mais proximo
rodaBotParaDirecaoJogador :: Jogador -> Estado -> Posicao -> Direcao                                    
rodaBotParaDirecaoJogador jog e (x1,y1)  | y > y1  = E
                                         | y < y1  = D
                                         | x < x1  = B
                                         | x > x1  = C
                                         | otherwise = D
                                    where (x,y) = posicaoJogador jog 

-- | Funcao que tenta mover para algum lado                              
movimentaParaAlgumLado :: Int -> Estado -> Maybe Direcao
movimentaParaAlgumLado n e | veSePodeMover bot E e = Just E
                           | veSePodeMover bot C e = Just C
                           | veSePodeMover bot B e = Just B
                           | veSePodeMover bot D e = Just D
                           | otherwise = Nothing
                        where bot = encontraIndiceLista n $ jogadoresEstado e
                                          
-- | Funcao que verifica se o bot se mover para a direcao que esta a pensar mover se leva um tiro                                          
veSeNaoLevaTiro :: Int -> Estado -> Direcao -> Bool
veSeNaoLevaTiro n e dir = not $ isJust closeShot
                  where jog = encontraIndiceLista n (jogadoresEstado e)
                        jogUpdated = jog {posicaoJogador = somaVetores (posicaoJogador jog) (direcaoParaVetor dir) } 
                        closeShot = tirosNasProximidades newEstado n (disparosEstado newEstado)
                        newEstado = e {jogadoresEstado = atualizaIndiceLista n jogUpdated (jogadoresEstado e)}

-- | Obtem o 'Jogador' mais proximo do bot                        
nearbyPlayer :: Jogador -> Estado -> Maybe Jogador
nearbyPlayer jog e = obtemJogadorAMinDistancia (minimoIntDaLista prox 1000) jog jogadores
                  where jogadores =  removeJogadoresLista jog $ jogadoresEstado e
                        prox = map (distanciaEntrePontos (posicaoJogador jog)) $ map (posicaoJogador) jogadores

-- | Retorna o 'Jogador' mais proximo do bot a distancia N                        
obtemJogadorAMinDistancia :: Int -> Jogador -> [Jogador] -> Maybe Jogador
obtemJogadorAMinDistancia _ _ [] = Nothing
obtemJogadorAMinDistancia n jog (h:t) = if distanciaEntrePontos (posicaoJogador jog) (posicaoJogador h) == n then Just h else obtemJogadorAMinDistancia n jog t

-- | Funcao que remove um 'Jogador' de uma lista de 'Jogador'
removeJogadoresLista :: Jogador -> [Jogador] -> [Jogador]
removeJogadoresLista _ [] = []
removeJogadoresLista jog (h:t) = if jog == h || vidasJogador h == 0 then removeJogadoresLista jog t else h:removeJogadoresLista jog t


--  TODO: Usar laser se estiver mais do que um tank alinhado? Ou por tras de muitas caixas (tipo 4?)


-- | Ataca os Jogadores mais proximos (ao alcance de 5) disparando canhoes 
atacaOsJogadores :: Int -> Estado -> Maybe Jogada -> Maybe Jogada
atacaOsJogadores _ _ (Just j) = Just j
atacaOsJogadores n e Nothing = if isJust nearPlayer && distanciaEntrePontos (posicaoJogador bot) (posicaoJogador $ fromJust nearPlayer) <=5 && vaiAcertar supostoDisparo (fromJust nearPlayer) e && not (disparouRecentemente n e  (disparosEstado e)) then Just $ Dispara Canhao else Nothing
                        where bot = encontraIndiceLista n (jogadoresEstado e)
                              nearPlayer = nearbyPlayer bot e
                              supostoDisparo = DisparoCanhao n (somaVetores (posicaoJogador bot) (direcaoParaVetor $ direcaoJogador bot)) (direcaoJogador bot)

-- | Verifica se o jogador disparou recentemente para evitar jogadas repetidas de forma a os canhoes nao se destruirem mutuamente
disparouRecentemente :: Int -> Estado -> [Disparo] -> Bool
disparouRecentemente _ _ [] = False
disparouRecentemente n e (h@DisparoCanhao{jogadorDisparo = jD , posicaoDisparo = pD ,direcaoDisparo = dD}:t) = if jD == n then recente else disparouRecentemente n e t
                                                                                                      where bot = encontraIndiceLista n (jogadoresEstado e)
                                                                                                            recente = subtraiVetores pD (direcaoParaVetor dD) == somaVetores (posicaoJogador bot) (direcaoParaVetor $ direcaoJogador bot) || disparouRecentemente n e t
disparouRecentemente n e (h:t) = disparouRecentemente n e t  


-- * Defende dos Tiros

-- | Defende dos tiros
defendeDosTiros :: Maybe Jogada -> Int -> Estado -> Maybe Jogada
defendeDosTiros (Just j) _ _  = Just j
defendeDosTiros j n e = atacaTiros n e $ defendeTiros n e j
   
-- | Defende dos tiros, tentando destruir os canhoes com canhoes ou lasers
atacaTiros ::  Int -> Estado -> Maybe Jogada -> Maybe Jogada
atacaTiros  _ _ (Just j) = Just j
atacaTiros  n e Nothing = if isJust closeShot then shotToDefend n e $ fromJust closeShot else Nothing
                        where closeShot = tirosNasProximidades e n (disparosEstado e)
                              jog = encontraIndiceLista n (jogadoresEstado e)         
                              
-- | Tenta disparar para se defender de disparos
shotToDefend :: Int -> Estado -> Disparo -> Maybe Jogada                              
shotToDefend n e d  = if direcaoJogador jog == dirOpostas (direcaoDisparo d) then (if jogadorEmFrenteAoOutro (n,jog) e d && lasersJogador jog > 0 then Just $ Dispara Laser else Just $ Dispara Canhao) else Just $ Movimenta $ dirOpostas (direcaoDisparo d) 
                  where jog = encontraIndiceLista n (jogadoresEstado e)

-- | Verifica se o 'Jogador' está a frente do jogador que disparou para caso esteja usar um laser                  
jogadorEmFrenteAoOutro :: (Int,Jogador) -> Estado -> Disparo -> Bool
jogadorEmFrenteAoOutro (n,jog) e d = vaiAcertar (DisparoLaser n  dispPos (dirOpostas $ direcaoDisparo d)) jogAAcertar e 
                              where dispPos = somaVetores (posicaoJogador jog) (direcaoParaVetor (direcaoJogador jog))
                                    jogAAcertar = encontraIndiceLista (jogadorDisparo d) (jogadoresEstado e)

-- | Defende dos tiros , tentando mover o jogador
defendeTiros ::  Int -> Estado -> Maybe Jogada -> Maybe Jogada
defendeTiros  _ _ (Just j) = Just j
defendeTiros  n e Nothing = if isJust closeShot then tryMove (fromJust closeShot) n e else Nothing
                        where closeShot = tirosNasProximidades e n (disparosEstado e)         
                              jog = encontraIndiceLista n (jogadoresEstado e)

-- | Tenta mover para desviar do tiro                              
tryMove :: Disparo -> Int -> Estado -> Maybe Jogada
tryMove d n e | podeMoverNaDirecaoDoJogador (direcaoJogador jog) (direcaoDisparo d)  && veSePodeMover jog (direcaoJogador jog) e = Just (Movimenta $ direcaoJogador jog)
              | veSePodeMover jog (posicaoParaDesvio (direcaoDisparo d)) e = Just (Movimenta (posicaoParaDesvio (direcaoDisparo d)))
              | veSePodeMover jog (dirOpostas $ posicaoParaDesvio (direcaoDisparo d)) e = Just (Movimenta (dirOpostas $ posicaoParaDesvio (direcaoDisparo d)))
              | otherwise = Nothing
               where jog = encontraIndiceLista n (jogadoresEstado e)

-- | Verifica se o 'Jogador' se pode mover na 'Direcao' pretendida               
veSePodeMover :: Jogador -> Direcao -> Estado -> Bool
veSePodeMover j d e = verificaMoverValido pos j e 
                  where pos = somaVetores  (posicaoJogador j) (direcaoParaVetor d)

-- | Verifica se pode Mover na direcao do Jogador                   
podeMoverNaDirecaoDoJogador:: Direcao -> Direcao -> Bool
podeMoverNaDirecaoDoJogador C B = False                  
podeMoverNaDirecaoDoJogador B C = False                  
podeMoverNaDirecaoDoJogador D E = False                  
podeMoverNaDirecaoDoJogador E D = False                  
podeMoverNaDirecaoDoJogador _ _  = True

-- | Retorna a 'Direcao' para desviar de tiros
posicaoParaDesvio :: Direcao -> Direcao
posicaoParaDesvio B = D
posicaoParaDesvio E = B
posicaoParaDesvio D = B
posicaoParaDesvio C = D                  
               

-- | Da me a direçao oposta de onde vem o disparo e o inteiro e usado para saber se ja tentou virar para algum lado                               
dirOpostas :: Direcao -> Direcao
dirOpostas E = D -- Se o disparo e vertical
dirOpostas D = E -- Se nao puder mover para a direita
dirOpostas C = B -- Se o disparo e horizontal
dirOpostas B = C -- Se nao puder mover para Baixo            

-- | Verifica se o disparo é vertical
dIsVertical :: Direcao -> Bool
dIsVertical C = True
dIsVertical B = True
dIsVertical _ = False




-- | Devolve me um tiro proximo se existir  // TODO : MUDAR A MANEIRA COMO O PROXIMO SE ENCONTRA. A DISTANCIA TEM DE SER 
tirosNasProximidades :: Estado -> Int -> [Disparo] -> Maybe Disparo
tirosNasProximidades _ _ [] = Nothing
tirosNasProximidades e n (h@DisparoCanhao {jogadorDisparo = jD, posicaoDisparo = pD , direcaoDisparo = dir}:t) = if jD /= n && vaiAcertar h jog e && distanciaEntrePontos pD (posicaoJogador jog) <= 3  then Just h else tirosNasProximidades e n t
                                                                                                            where tankBox = tirosProxTank (posicaoJogador jog) 8
                                                                                                                  jog = encontraIndiceLista n (jogadoresEstado e)
tirosNasProximidades e n (h:t) = tirosNasProximidades e n t
                                                                                                            --tirosNasProximidades p (h@DisparoChoque{jogadorDisparo = jD,tickDisparo = _}:t) = tirosNasProximidades p t CASO QUEIRA FAZER ALGO SE HOUVER UM CHOQUE AO PE

-- | Verifica se o 'Disparo' podera acertar no 'Jogador'                                                                                                            
vaiAcertar :: Disparo -> Jogador -> Estado -> Bool
vaiAcertar d j e = vaiAcertarJogador (dIsVertical $ direcaoDisparo d) posD posJ && not (noCaminhoBlocoIndestrutivel posJ (direcaoDisparo d) (posD) e )
            where posD = getPosDisp (direcaoDisparo d ) (posicaoDisparo d)
                  posJ = pos4Tank $ posicaoJogador j      

-- | Verifica se no caminho do disparo há blocos indestrutiveis ou blocos destrutiveis                  
noCaminhoBlocoIndestrutivel :: [Posicao] -> Direcao -> (Posicao,Posicao) -> Estado -> Bool
noCaminhoBlocoIndestrutivel [] _ _ _ = False
noCaminhoBlocoIndestrutivel l dir (pos,pos1) e = if peca pos == Bloco Indestrutivel || peca pos1 == Bloco Indestrutivel || peca pos == Bloco Destrutivel || peca pos1 == Bloco Destrutivel then not (aBater l (pos,pos1)) else  if aBater l (pos,pos1) then False else noCaminhoBlocoIndestrutivel l dir newPos e 
                                                where peca a = encontraPosicaoMatriz a (mapaEstado e)
                                                      newPos = (somaVetores pos (direcaoParaVetor dir),somaVetores pos1 (direcaoParaVetor dir))
                      
-- | Verifica se o disparo está a bater no tank                                                      
aBater :: [Posicao] -> (Posicao,Posicao) -> Bool
aBater [] _ = False
aBater (h:t) (pos,pos1) = h == pos || h == pos1 || aBater t (pos,pos1)

-- | Verifica se vai acertar no Jogador
vaiAcertarJogador :: Bool ->  (Posicao,Posicao) -> [Posicao] -> Bool
vaiAcertarJogador _ _ [] = False    
vaiAcertarJogador False posD@((x,y),(x1,y1)) ((a,b):t) =  x == a && y /= b || x1 == a && y1 /= b || vaiAcertarJogador False posD t    
vaiAcertarJogador True  posD@((x,y),(x1,y1)) ((a,b):t) =  x /= a && y == b || x1 /= a && y1 == b || vaiAcertarJogador True posD t


-- | Retorna os tiros proximos do tank
tirosProxTank :: Posicao -> Int -> [Posicao]
tirosProxTank _ 0 = []
tirosProxTank (x,y) n = (x+n,y):(x,y+n):(x-n,y):(x,y-n):tirosProxTank (x,y) (n-1) 

-- | Devolve o minimo inteiro de uma lista
minimoIntDaLista :: [Int] -> Int -> Int
minimoIntDaLista [] n = n
minimoIntDaLista (h:t) n = minimoIntDaLista t (min n h)

-- | Retorna a distancia entre os dois pontos , util para saber se o disparo esta perto
distanciaEntrePontos :: Posicao -> Posicao -> Int
distanciaEntrePontos (x,y) (x1,y1) = round $ sqrt $ fromIntegral(x1-x)^2 + fromIntegral(y1-y)^2 
                                    
-- | Devolve as 4 posicoes relativas ao tank do jogador na posicao
pos4Tank :: Posicao -> [Posicao]
pos4Tank (x,y) = [(x,y),(x+1,y),(x+1,y+1),(x,y+1)]