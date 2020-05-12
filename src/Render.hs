{-|
Module : Render
Description : Modulo que trata de renderizar o jogo 
-}
module Render where 
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import LI11819
import Mapa
import Menu
import Jogador
import Controls
import Data.Maybe
import Tarefa1_2018li1g101
import EstadosInicial
import Tarefa0_2018li1g101

-- | Estado Inicial
estadoGlossInicial :: Int -> (Picture,Picture) -> EstadoGloss
estadoGlossInicial n (bI,f) = EstadoGloss estadoInicial estadoInicial n (bI,f) (mkMapa estadoInicial bI f) Start Nothing [] 0
                        where estadoInicial = encontraIndiceLista n listaMapas 

-- | Desenha o Estado na Picture para o Gloss representar
desenhaEstadoGloss :: ([Picture],[Picture],[Picture],Picture,Picture,[Picture])-> EstadoGloss  -> Picture
desenhaEstadoGloss info eG  = desenhaGameState (gameState eG) info eG

-- | Desenha a picture relativa ao estado de Jogo em que está
desenhaGameState :: GameStatus -> ([Picture],[Picture],[Picture],Picture,Picture,[Picture]) -> EstadoGloss -> Picture
desenhaGameState Start _ _ = menuInicio
desenhaGameState InPause _ _ = menuPausa
desenhaGameState InGame (tank,disp,laser,pD,pI,choque) eG =  pictures [pic,mkWallDest e pD,mkDisparos e disp laser choque,mkJogador e tank, mkinfo eG]
                                        where pic = imagemGloss eG
                                              e = currentEstado eG
desenhaGameState Won _ eG = menuGanhou n
                        where jogWon = alguemGanhou (jogadoresEstado $ currentEstado eG)
                              n = jogadorGanhou (jogadoresEstado $ currentEstado eG) (fromJust jogWon)
           
-- | Retorna o Id do 'Jogador' que ganhou                              
jogadorGanhou :: [Jogador] -> Jogador  -> Int
jogadorGanhou [] _ = 0 
jogadorGanhou (h:t) jog = if h == jog then jogadorGanhou [] jog else 1+ jogadorGanhou t jog                     

                                             