{-|
Module : Janela
Description : Modulo que trata das Propriedades da janela
-}
module Janela where 
import Graphics.Gloss
import LI11819
-- * Janela

-- | FrameRate
fps :: Int
fps = 15

-- | Características da Janela do Jogo
window :: Display
window = InWindow
    "Tank" -- título da janela
    resolucaoEcra -- dimensão da janela
    (10,10) -- posição no ecrã

-- | Cor de fundo da Janela do Jogo
background :: Color
background = greyN 0.9
