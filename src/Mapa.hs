{-|
Module : Mapa
Description : Modulo que trata de desenhar o mapa
-}
module Mapa where
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import LI11819
import Tarefa1_2018li1g101
import Tarefa0_2018li1g101
import Data.Maybe
 

-- | Desenha o Mapa
mkMapa :: Estado -> Picture -> Picture -> Picture
mkMapa estadoInicial bI f = pictures ([fundo] ++  vazia ++ [mkWallIndest estadoInicial bI] ++ [bordas]) 
        where pos = obtemMapa (mapaEstado estadoInicial) (Bloco Indestrutivel) (0,0)
              posVazia = obtemMapa (mapaInicial (dimensaoMatriz (mapaEstado estadoInicial))) Vazia (1,0)
              bordas = mkLinha estadoInicial
              fundo = mkfundo f
              vazia = map mkVazia posVazia
      

-- | Desenha as Paredes Indestrutiveis
mkWall :: Picture -> (Int,Int) -> Picture
mkWall bI (x,y) =  Translate x1 y1 bI
            where (x1,y1) = toReferential  (x,y)

-- | Desenha a parte vazia do Mapa lol     
mkVazia :: (Int,Int) -> Picture
mkVazia (x,y) = translate (xRef-13) (yRef+12) polygon --Translate xRef yRef polygon
          where polygon =  Color (greyN 1) $ Polygon (rectanglePath 25 25)
                (xRef,yRef) = toReferential  (x,y)

-- | Desenha as Paredes Destrutiveis atraves de um map das posições onde há um bloco destrutivel
mkWallDest :: Estado -> Picture -> Picture
mkWallDest e pD = pictures(map (mkDestrutivel pD) lpos)
               where lpos = obtemMapa (mapaEstado e) (Bloco Destrutivel) (1,0)

-- | Desenha a Parede Destrutivel 
mkDestrutivel :: Picture -> (Int,Int) -> Picture
mkDestrutivel pD (x,y)  = translate (x1-12.5) (y1+12.5) pD
                    where (x1,y1) = toReferential  (x,y)
                    
-- | Desenha um blocos indestrutivel na posicao (x,y)
mkIndestrutivel :: Picture -> (Int,Int) -> Picture 
mkIndestrutivel pI (x,y) = translate (x1-12.5) (y1+12.5) pI 
                              where (x1,y1) = toReferential (x,y)                  

-- | Desenha os blocos indestrutiveis dentro do mapa                 
mkWallIndest :: Estado -> Picture -> Picture
mkWallIndest e pI = pictures(map (mkIndestrutivel pI) lpos)
                        where lpos = obtemMapa (tiraBordas (0,0) (mapaEstado e)) (Bloco Indestrutivel) (1,0)

-- | Obtem Cordenadas Do Mapa relativamente a um tipo de Peca
tiraBordas :: (Int,Int) -> Mapa -> Mapa
tiraBordas _ [] = []
tiraBordas (x,y) m =if acabou then m else (if final then proximalinha else (if borda then atualizaVazia else sematualizar))
                              where final = (y == snd ((dimensaoMatriz m)))
                                    proximalinha = tiraBordas (x+1,0) m
                                    borda = eBordaMatriz (x,y) m
                                    atualizaVazia = tiraBordas (x,y+1) (atualizaPosicaoMatriz (x,y) Vazia m)
                                    sematualizar = tiraBordas (x,y+1) m 
                                    acabou = (x == fst ((dimensaoMatriz m)))

-- | Obtem um conjunto de posicoes relativas a um tipo de peca de um mapa 
obtemMapa :: Mapa -> Peca -> (Int,Int) -> [(Int,Int)]
obtemMapa [] _ _ = []
obtemMapa ([]:t) peca (x,y) = obtemMapa t peca (0,y+1) 
obtemMapa e@((h:hs):t) peca (x,y) = if h == peca then (x,y):obtemMapa (hs:t) peca (x+1,y) else obtemMapa (hs:t) peca (x+1,y)
                                                                        
-- | Atualiza coordenadas para o referencial 0 se for um jogador outra coisa se for objetos do mapa
toReferential ::(Int,Int) -> (Float,Float)
toReferential (x,y) = (realToFrac ((tamanhoBloco*x)+posInicialX),realToFrac(posInicialY - (tamanhoBloco*y)))

-- | Desenha a linha do jogo
mkLinha :: Estado -> Picture
mkLinha estado = line (posicaoBorda mapa)
                  where mapa = dimensaoMatriz (mapaEstado estado)

-- | Retorna as Posicoes das bordas ja no referencial de forma a desenhar a linha
posicaoBorda:: Dimensao -> [(Float,Float)]
posicaoBorda d =  [toReferential (0,0) , toReferential (b,0),toReferential (b,a),toReferential (0,a),toReferential(0,0)]
                  where a = fst d -2
                        b = snd d -2


-- |  Recebe o estadoGloss e mostra determinados aspetos relativos ao jogo. Eg: vidas , n de choques, etc
mkinfo :: EstadoGloss -> Picture
mkinfo eG = pictures ([players] ++ [info])
            where players = infoJog (jogadoresEstado e) 
                  info = generalizada eG 
                  e = currentEstado eG

-- | faz a info da parte do mapa e da informação geral   
generalizada :: EstadoGloss -> Picture
generalizada eG = pictures (box ++ box2 ++ [info])
                  where caixa = color (greyN 0.9) $ Polygon (posicaoBorda (5,14))
                        caixa2 = line (posicaoBorda (5,14))
                        box = [translate 716.0 70 (caixa)] ++ [translate 716.0 70 (caixa2)]
                        box2 = [translate 716.9 (-600.0) (caixa)] ++ [translate 716.0 (-600.0) (caixa2)]
                        info = infogene eG 
                        e = currentEstado eG

-- | info do mapa que esta e de certos comandos
infogene :: EstadoGloss -> Picture 
infogene eG = pictures ([info1] ++ [info2] ++ [info3] ++ [info4] ++ [info5])
            where x = 0.00508 * (fst (toReferential (resolucaoEcra)))
                  info1 = translate x 370 $ scale 0.09 0.09 $ text ("          MAPA : " ++ (show (mapaInt eG +1)))
                  info2 = translate x 350 $ scale 0.09 0.09 $ text "     PROXIMO MAPA : SPACE"
                  info3 = translate x 330 $ scale 0.09 0.09 $ text "    PAUSA : P     MENU : M"
                  info4 = translate x (-300) $ scale 0.09 0.09 $ text texto
                  info5 = translate x (-330) $ scale 0.09 0.09 $ text "P1 : Z   P2 : X   P3 : C   P4 : V "
                  texto = if isJust $ botIsActive eG then "         BOT ATIVADO N:"++ (show $ (fromJust (botIsActive eG)+1))  else "         ATIVAR BOTS "




-- | faz as infos dos jogadores 
infoJog ::  [Jogador] -> Picture 
infoJog [] = blank
infoJog (a:b:c:d:t) = pictures(box1 ++ box2 ++ box3 ++ box4 ++ [jogador1 a] ++ [jogador2 b] ++ [jogador3 c] ++ [jogador4 d])
                        where caixa = color (greyN 0.9) $ Polygon (posicaoBorda (8,14))
                              caixa2 = line (posicaoBorda (8,14))
                              box1 = [translate 716.0 0.0 (caixa)] ++ [translate 716.0 0.0 (caixa2)]
                              box2 = [translate 716.0 (-150.0) (caixa)] ++ [translate 716.0 (-150.0) (caixa2)]
                              box3 = [translate 716.0 (-300.0) (caixa)] ++ [translate 716.0 (-300.0) (caixa2)]
                              box4 = [translate 716.0 (-450.0) (caixa)] ++ [translate 716.0 (-450.0) (caixa2)]
                              
                              

-- |o quadrado de um jogador
jogador1 :: Jogador -> Picture
jogador1 j = pictures([info1] ++ [info2] ++ [info3] ++ [info4])
            where x = 0.00508 * (fst (toReferential (resolucaoEcra)))
                  y = - (0.01423 * (snd (toReferential (resolucaoEcra))))
                  info1 = color red $ (translate (x) (y) $ scale 0.1 0.1 $ text "P1")
                  info4 = (translate (x) (y) $ scale 0.1 0.1 $ text "       KEYS = ARROWS , . -")
                  info2 = (translate (x) (y - 40.0) $ scale 0.1 0.1 $ text ("CANHOES : Infinitos   LASERS : " ++ (show (lasersJogador j))))
                  info3 = (translate (x) (y- 80.0) $ scale 0.1 0.1 $ text ("CHOQUES : " ++ (show (choquesJogador j)) ++"     VIDAS : " ++ (show (vidasJogador j))))



-- |o quadrado de um jogador
jogador2 :: Jogador -> Picture
jogador2 j = pictures([info1] ++ [info2] ++ [info3] ++ [info4])
            where x = 0.00508 * (fst (toReferential (resolucaoEcra)))
                  y = (- (0.01423 * (snd (toReferential (resolucaoEcra))))-150.0)
                  info1 = color yellow $ (translate x (y) $ scale 0.1 0.1 $ text "P2")
                  info4 = (translate x (y) $ scale 0.1 0.1 $ text "       KEYS = W A S D 123")
                  info2 = (translate x (y - 40.0) $ scale 0.1 0.1 $ text ("CANHOES : Infinitos  LASERS : " ++ (show (lasersJogador j))))
                  info3 = (translate x (y - 80.0) $ scale 0.1 0.1 $ text ("CHOQUES : " ++ (show (choquesJogador j)) ++  "     VIDAS : " ++ (show (vidasJogador j))))


-- |o quadrado de um jogador
jogador3 :: Jogador -> Picture
jogador3 j = pictures([info1] ++ [info2] ++ [info3] ++ [info4])
            where x = 0.00508 * (fst (toReferential (resolucaoEcra)))
                  y = (- ( 0.01423 * (snd (toReferential (resolucaoEcra)))) - 300)
                  info1 = color green $ (translate x (y) $ scale 0.1 0.1 $ text "P3")
                  info4 = (translate x (y) $ scale 0.1 0.1 $ text "       KEYS = T F G H 456")
                  info2 = (translate x (y - 40.0) $ scale 0.1 0.1 $ text ("CANHOES : Infinitos  LASERS : " ++ (show (lasersJogador j))))
                  info3 = (translate x (y-80.0) $ scale 0.1 0.1 $ text ("CHOQUES : " ++ (show (choquesJogador j)) ++  "     VIDAS : " ++ (show (vidasJogador j))))


-- |o quadrado de um jogador
jogador4 :: Jogador -> Picture
jogador4 j = pictures([info1] ++ [info2] ++ [info3] ++ [info4])
            where x = 0.00508 * (fst (toReferential (resolucaoEcra)))
                  y = (- (0.01423 * (snd (toReferential (resolucaoEcra)))) - 450)
                  info1 = color white $ (translate x (y) $ scale 0.1 0.1 $ text "P4")
                  info4 = (translate x (y) $ scale 0.1 0.1 $ text "       KEYS = J I K L 789")
                  info2 = (translate x (y - 40.0) $ scale 0.1 0.1 $ text ("CANHOES : Infinitos  LASERS : " ++ (show (lasersJogador j))))
                  info3 = (translate x (y - 80.0) $ scale 0.1 0.1 $ text ("CHOQUES : " ++ (show (choquesJogador j)) ++ "     VIDAS : " ++ (show (vidasJogador j))))


-- | Desenha a imagem de fundo
mkfundo :: Picture -> Picture
mkfundo = scale 0.8 0.7