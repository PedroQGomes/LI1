
-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Tarefa5_2018li1g101 where
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Tarefa4_2018li1g101
import Tarefa3_2018li1g101
import Tarefa2_2018li1g101
import Tarefa1_2018li1g101
import Tarefa0_2018li1g101
import LI11819
import Mapa
import Controls
import TickRun
import Janela
import Render



-- | Ficheiros com as texturas dos tanks
tankFiles :: [FilePath]
tankFiles = ["images/tank_red.bmp","images/tank_yellow.bmp","images/tank_green.bmp","images/tank_white.bmp"]

-- | Ficheiros com as texturas dos Disparos Canhoes
dispFiles :: [FilePath]
dispFiles = ["images/disp_red.bmp","images/disp_yellow.bmp","images/disp_green.bmp","images/disp_black.bmp"]

-- | Ficheiros com as texturas dos Disparos Laser
laserFiles :: [FilePath]
laserFiles = ["images/laser_red.bmp","images/laser_yellow.bmp","images/laser_green.bmp","images/laser_black.bmp"]

-- | Ficheiros com as texturas dos Disparos Choque
choqueFiles :: [FilePath]
choqueFiles = ["images/choque_red.bmp","images/choque_yellow.bmp","images/choque_green.bmp","images/choque_azul.bmp"]

-- | Função principal da Tarefa 5. Executa o Jogo
main :: IO ()
main = do 
    tank <- mapM loadBMP  tankFiles
    pD <- loadBMP "images/par_dest.bmp"
    pI <- loadBMP "images/bloco_ind.bmp"
    disp<- mapM loadBMP dispFiles
    laser <- mapM loadBMP laserFiles
    fundo <- loadBMP "images/fundo.bmp"
    choque <- mapM loadBMP choqueFiles                           -- Load das texturas                 
    play window                                                  -- Janela Principal onde corre o jogo ('Window')
        background                                               -- Cor de fundo da janela 
        fps                                                      -- Frame rate
        (estadoGlossInicial 0 (pI,fundo))                        -- Estado Inicial
        (desenhaEstadoGloss (tank,disp,laser,pD,pI,choque))      -- Desenha o 'Estado' do Jogo
        reageEventoGloss                                         -- Reage a um evento (Pressionar Teclas etc) ('Event')
        reageTempoGloss                                          -- Reage ao passar do tempo ('Ticks')
    

