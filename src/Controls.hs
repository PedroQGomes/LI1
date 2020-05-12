{-|
Module : Controls
Description : Modulo que trata dos controlos do jogo
-}
module Controls where 
import Graphics.Gloss.Interface.Pure.Game
import LI11819
import EstadosInicial
import Mapa
import Tarefa0_2018li1g101
import Tarefa1_2018li1g101
import Tarefa2_2018li1g101
import Janela



-- | Funçao ativada quando há um evento No Gloss
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss (EventKey (Char 'm') Down _ _ ) estado = if gameState estado /= Start  then estado {currentEstado = estadoInicial estado,gameState = Start ,botIsActive = Nothing} else estado
reageEventoGloss (EventKey (Char 'p') Down _ _ ) estado = if gameState estado == InGame then estado {gameState = InPause} else estado { gameState = InGame}
reageEventoGloss (EventKey (SpecialKey KeySpace) Down _ _) estado = if gameState estado == InGame then mudaDeEstado estado else estado
reageEventoGloss (EventKey (Char 'z') Down _ _) e = e{botIsActive = if botIsActive e == Just 0 then Nothing else Just 0 }
reageEventoGloss (EventKey (Char 'x') Down _ _) e = e{botIsActive = if botIsActive e == Just 1 then Nothing else Just 1 }
reageEventoGloss (EventKey (Char 'c') Down _ _) e = e{botIsActive = if botIsActive e == Just 2 then Nothing else Just 2 }
reageEventoGloss (EventKey (Char 'v') Down _ _) e = e{botIsActive = if botIsActive e == Just 3 then Nothing else Just 3 }

reageEventoGloss event@(EventKey key Up _ _) estado = if boolKey event then estado {teclas_press = retiraTeclaLista key (teclas_press estado)} else estado 
reageEventoGloss event@(EventKey key Down _ _) estado = if boolKey event && gameState estado == InGame then estado {teclas_press = key:teclas_press estado} else estado 
reageEventoGloss _ e = e

 
-- | Retira tecla da Lista quando a mesma é levantada
retiraTeclaLista :: Key -> [Key] -> [Key] 
retiraTeclaLista _ [] = []
retiraTeclaLista x (h:t) = if x==h then retiraTeclaLista x t else h:retiraTeclaLista x t

-- | Verifica se a tecla é para fazer mover o Jogador
boolKey :: Event -> Bool
boolKey (EventKey (SpecialKey key) _ _ _) = True
boolKey (EventKey (Char key) _ _ _ ) = True
boolKey _ = False

-- | Funcao que move o Jogador baseado nas teclas pressionadas
moveJogador :: Int -> Maybe Int -> [Key] -> Estado -> Estado
moveJogador _ _ [] e = e
moveJogador tick n (h:t) e  = keyEvent n h $ moveJogador tick n t e

-- | Verifica se o jogador e um Bot
jogadorEBot :: Int -> Maybe Int -> Bool
jogadorEBot _ Nothing = False
jogadorEBot n (Just a) = n == a

-- | Mapeamento das teclas do Jogo e a sua função
keyEvent :: Maybe Int -> Key -> Estado -> Estado
keyEvent n (SpecialKey KeyUp) e = if jogadorEBot 0 n then e else jogada 0 (Movimenta C) e
keyEvent n (SpecialKey KeyDown) e = if jogadorEBot 0 n then e else  jogada 0 (Movimenta B) e
keyEvent n (SpecialKey KeyLeft) e = if jogadorEBot 0 n then e else  jogada 0 (Movimenta E) e
keyEvent n (SpecialKey KeyRight) e = if jogadorEBot 0 n then e else  jogada 0 (Movimenta D) e
keyEvent n (Char ',') e = if jogadorEBot 0 n then e else  jogada 0 (Dispara Canhao) e
keyEvent n (Char '.') e = if jogadorEBot 0 n then e else  jogada 0 (Dispara Laser) e
keyEvent n (Char '-') e = if jogadorEBot 0 n then e else  jogada 0 (Dispara Choque) e
keyEvent n (Char 'w' ) e = if jogadorEBot 1 n then e else  jogada 1 (Movimenta C) e
keyEvent n (Char 's') e = if jogadorEBot 1 n then e else jogada 1 (Movimenta B) e
keyEvent n (Char 'a') e = if jogadorEBot 1 n then e else jogada 1 (Movimenta E) e
keyEvent n (Char 'd') e = if jogadorEBot 1 n then e else jogada 1 (Movimenta D) e
keyEvent n (Char '1') e = if jogadorEBot 1 n then e else jogada 1 (Dispara Canhao) e
keyEvent n (Char '2') e = if jogadorEBot 1 n then e else jogada 1 (Dispara Laser) e
keyEvent n (Char '3') e = if jogadorEBot 1 n then e else jogada 1 (Dispara Choque) e
keyEvent n (Char 't') e = if jogadorEBot 2 n then e else jogada 2 (Movimenta C) e
keyEvent n (Char 'g') e = if jogadorEBot 2 n then e else jogada 2 (Movimenta B) e
keyEvent n (Char 'f') e = if jogadorEBot 2 n then e else jogada 2 (Movimenta E) e
keyEvent n (Char 'h') e = if jogadorEBot 2 n then e else jogada 2 (Movimenta D) e
keyEvent n (Char '4') e = if jogadorEBot 2 n then e else jogada 2 (Dispara Canhao) e
keyEvent n (Char '5') e = if jogadorEBot 2 n then e else jogada 2 (Dispara Laser) e
keyEvent n (Char '6') e = if jogadorEBot 2 n then e else jogada 2 (Dispara Choque) e
keyEvent n (Char 'i' ) e = if jogadorEBot 3 n then e else jogada 3 (Movimenta C) e
keyEvent n (Char 'k') e = if jogadorEBot 3 n then e else  jogada 3 (Movimenta B) e
keyEvent n (Char 'j') e = if jogadorEBot 3 n then e else  jogada 3 (Movimenta E) e
keyEvent n (Char 'l') e = if jogadorEBot 3 n then e else  jogada 3 (Movimenta D) e
keyEvent n (Char '7') e = if jogadorEBot 3 n then e else  jogada 3 (Dispara Canhao) e
keyEvent n (Char '8') e = if jogadorEBot 3 n then e else  jogada 3 (Dispara Laser) e
keyEvent n (Char '9') e = if jogadorEBot 3 n then e else  jogada 3 (Dispara Choque) e
keyEvent _ _ s = s

