{-|
Module : TickRun
Description : Modulo que trata de reagir ao tempo
-}
module TickRun where
import LI11819
import Tarefa4_2018li1g101
import Tarefa6_2018li1g101
import Jogador
import Controls
import Data.Maybe

-- | Funcao que reage ao passar do tempo tendo em conta os fps
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss n e = if gameState e == InGame then e { currentEstado =  jogaBot e $  moveJogador (timer e) (botIsActive e) (teclas_press e) $ tick (currentEstado e), timer = timer e +1, gameState = stateAlguemGanhou (jogadoresEstado $ currentEstado e)  } else e

-- | Funcao que verifica se o bot esta ativo
jogaBot :: EstadoGloss -> Estado -> Estado 
jogaBot eG e = if isJust (botIsActive eG) then jogadaBot (fromJust (botIsActive eG)) e else e           
            
