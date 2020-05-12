{-|
Module : Jogador
Description : Modulo que trata das funçoes do Jogador
-}
module Jogador where       
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import LI11819
import Tarefa2_2018li1g101
import Tarefa0_2018li1g101
import Tarefa6_2018li1g101
import Data.Maybe

-- * Funcoes relativas a parte do Jogador do Gloss

-- | Funcao que retorna o state 'Won' se algum 'Jogador' ganhou
stateAlguemGanhou :: [Jogador] -> GameStatus
stateAlguemGanhou [] = Start
stateAlguemGanhou l = if isJust jog then Won else InGame
                  where jog = alguemGanhou l

-- | Funcao que verifica se algum 'Jogador' Ganhou                  
alguemGanhou :: [Jogador] -> Maybe Jogador
alguemGanhou [] = Nothing
alguemGanhou l = if length (filter (\x -> vidasJogador x > 0) l) == 1 then returnJogadorVivo l else Nothing 

-- | Funcao que retorna o 'Jogador' vivo (que ganhou)
returnJogadorVivo :: [Jogador] -> Maybe Jogador
returnJogadorVivo [] = Nothing
returnJogadorVivo (h:t) = if vidasJogador h > 0 then Just h else returnJogadorVivo t

-- | Faz a jogada do Bot
jogadaBot :: Int -> Estado -> Estado
jogadaBot n e = if isJust botJogada then jogada n (fromJust botJogada) e else e
        where botJogada = bot n e

-- | Funcao que desenha os jogadores no jogo
mkJogador :: Estado -> [Picture] -> Picture
mkJogador e tank = pictures(map (mkTank tank e) jog )
                where jog = jogadoresVivos (jogadoresEstado e)
                  
         
-- | Funcao que retorna uma lista dos jogadores que estao vivos                
jogadoresVivos :: [Jogador] -> [Jogador]  
jogadoresVivos [] = []
jogadoresVivos (h:t) = if vidasJogador h > 0 then h:jogadoresVivos t else jogadoresVivos t             

-- | Funcao que desenha os disparos no jogo
mkDisparos :: Estado -> [Picture] -> [Picture] -> [Picture] -> Picture
mkDisparos e disp l choque = pictures(map (mkDisparo (disp,l,choque) e) (disparosEstado e))

-- | Funcao que desenha um disparo no jogo para construir uma lista de todos os disparos
mkDisparo :: ([Picture],[Picture],[Picture]) -> Estado -> Disparo  -> Picture
mkDisparo (c,l,choque) e d | tipoDisparo  d == Canhao = Translate x y (imagemNaDirecao (c !! jogadorDisparo d))
                           | tipoDisparo  d == Laser  = pictures (map (\ (a,b) -> Translate a b (laserImage dir) ) laserParaReferential)
                           | tipoDisparo  d == Choque = desenhaChoque d (choque !! jogadorDisparo d) e
                           | otherwise = blank
                where (x,y) = toReferentialBala dir pos
                      dir = direcaoDisparo d
                      pos = posicaoDisparo d
                      laserImage f = if f == D || f == E then scale 3.5 1 (imagemNaDirecao (l !! jogadorDisparo d)) else scale 1 3.5 (imagemNaDirecao (l !! jogadorDisparo d))
                      imagemNaDirecao  = rodaImagem dir 
                      laserParaReferential = map (toReferentialLaser dir) (laserPos d e)

-- | Funcao que retorna as posicoes onde o laser passa para o desenhar                      
laserPos :: Disparo -> Estado -> [Posicao]
laserPos d e = if  isVazia pos && isVazia pos1 then pos:laserPos disp e else []
            where (pos,pos1) = getPosLaser (direcaoDisparo d) (posicaoDisparo d)
                  isVazia pos = encontraPosicaoMatriz pos (mapaEstado e) /= Bloco Indestrutivel
                  disp = d{posicaoDisparo = somaVetores (posicaoDisparo d) (direcaoParaVetor (direcaoDisparo d))}
     
-- | Funcao que desenha o Choque                  
desenhaChoque :: Disparo -> Picture -> Estado -> Picture
desenhaChoque d choque e = Translate  xRef (yRef) choqueG
                where jog = encontraIndiceLista (jogadorDisparo d) (jogadoresEstado e)
                      (x,y) = posicaoJogador jog 
                      (xRef,yRef) = toReferentialTank (x,y)
                      choqueG = scale 1.5 1.5 choque

-- | Retorna ambas as Posicoes do Disparo
getPosLaser :: Direcao -> Posicao -> (Posicao,Posicao)
getPosLaser C (x,y) = ((x,y),(x,y+1))
getPosLaser B (x,y) = ((x+1,y),(x+1,y+1))
getPosLaser D (x,y) = ((x,y+1),(x+1,y+1))
getPosLaser E (x,y) = ((x,y),(x+1,y))                  

-- | Funcao que retorna o tipo de 'Arma' em Funcao do tipo de 'Disparo'
tipoDisparo :: Disparo -> Arma
tipoDisparo DisparoCanhao {jogadorDisparo = _ , posicaoDisparo = _ , direcaoDisparo = _} = Canhao                      
tipoDisparo DisparoLaser {jogadorDisparo = _ , posicaoDisparo = _ , direcaoDisparo = _} = Laser                      
tipoDisparo DisparoChoque {jogadorDisparo = _ , tempoDisparo = _} = Choque                      

-- | Funcao que desenha o tank do jogador no jogo na posição onde ele se encontrar                      
mkTank :: [Picture] -> Estado -> Jogador -> Picture
mkTank tank e jog = Translate x y (rodaImagem dir (tank !! indJog))
                  where (x,y) = toReferentialTank pos
                        dir = direcaoJogador jog
                        pos = posicaoJogador jog
                        indJog = retornaIndiceJogador jog $ jogadoresEstado e

-- | Funcao que retorna o indice de um 'Jogador' de uma lista de '[Jogador]'                        
retornaIndiceJogador :: Jogador -> [Jogador] -> Int
retornaIndiceJogador _ [] = 0
retornaIndiceJogador j (h:t) = if h /= j then 1+retornaIndiceJogador j t else retornaIndiceJogador j []                       

-- | Roda a imagem do tanque consoante a direção para onde está virado
rodaImagem :: Direcao -> Picture -> Picture
rodaImagem D p= rotate 270.0 (Color red p)
rodaImagem B p= rotate 0.0 (Color red p)
rodaImagem C p= rotate 180.0 (Color red p)
rodaImagem E p= rotate 90.0 (Color red p)

-- | Mete a posicao do Tank do Jogador numa posicao do referencial do Gloss
toReferentialTank :: Posicao -> (Float,Float)
toReferentialTank (x,y) = (realToFrac (posInicialX +(y*tamanhoBloco)),realToFrac (posInicialY-(x*tamanhoBloco)))

-- | Mete a posicao da Bala numa posicao para desenhar no gloss
toReferentialBala :: Direcao -> Posicao -> (Float,Float)
toReferentialBala C (x,y)  = (realToFrac (posInicialX+(y*tamanhoBloco)),realToFrac (posInicialY-10-(x*tamanhoBloco)))
toReferentialBala D (x,y)  = (realToFrac (posInicialX-10 +(y*tamanhoBloco)),realToFrac (posInicialY-(x*tamanhoBloco)))
toReferentialBala B (x,y)  = (realToFrac (posInicialX+(y*tamanhoBloco)),realToFrac (posInicialY+10-(x*tamanhoBloco)))
toReferentialBala E (x,y)  = (realToFrac (posInicialX+10+(y*tamanhoBloco)),realToFrac (posInicialY-(x*tamanhoBloco)))

-- | Mete a posicao do Laser numa posicao para desenhar no gloss
toReferentialLaser :: Direcao -> Posicao -> (Float,Float)
toReferentialLaser C (x,y)  = (realToFrac (posInicialX+(y*tamanhoBloco)),realToFrac (posInicialY-(x*tamanhoBloco)))
toReferentialLaser D (x,y)  = (realToFrac (posInicialX-25+(y*tamanhoBloco)),realToFrac (posInicialY-(x*tamanhoBloco)))
toReferentialLaser B (x,y)  = (realToFrac (posInicialX+(y*tamanhoBloco)),realToFrac (posInicialY+25-(x*tamanhoBloco)))
toReferentialLaser E (x,y)  = (realToFrac (posInicialX+(y*tamanhoBloco)),realToFrac (posInicialY-(x*tamanhoBloco)))
