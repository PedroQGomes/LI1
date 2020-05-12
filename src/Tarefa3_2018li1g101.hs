-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2018li1g101 where
import Tarefa1_2018li1g101
import Tarefa2_2018li1g101 
import Data.List
import Data.List.Split
import Tarefa0_2018li1g101
import Data.Char
import LI11819

-- * Testes

-- | Testes unitários da Tarefa 3
--
-- Cada teste é um 'Estado'.




testesT3 :: [Estado]
testesT3 = [Estado {mapaEstado = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]],jogadoresEstado = [Jogador {posicaoJogador = (5,1), direcaoJogador = B, vidasJogador = 0, lasersJogador = 3, choquesJogador = 3}], disparosEstado = [DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (6,1), direcaoDisparo = B},DisparoChoque {jogadorDisparo = 0, tempoDisparo = 10 }]}]







-- * Funções principais da Tarefa 3.


-- | Comprime um 'Estado' para formato textual.
comprime :: Estado -> String 
comprime Estado{mapaEstado = mE, jogadoresEstado = jE, disparosEstado = dE} = mapa  ++ jogadores ++ disparos 
                                                                              where mapa = comprimeMapa mE
                                                                                    jogadores = comprimeJogadores jE
                                                                                    disparos = comprimeDisparos dE

-- * Disparos


-- |  Comprime os disparos em formato textual.
comprimeDisparos :: [Disparo] -> String
comprimeDisparos (DisparoCanhao {jogadorDisparo = jD , posicaoDisparo = pD , direcaoDisparo = dD}:t) 
                 = "a" ++ jogador ++show dD ++  posicaoString pD ++ comprimeDisparos t
                 where jogador = if jD >0 then  show jD else []    
comprimeDisparos (DisparoLaser {jogadorDisparo = jD , posicaoDisparo = pD , direcaoDisparo = dD}:t)
                  = "l" ++ jogador ++ show dD ++  posicaoString pD ++ comprimeDisparos t
                  where jogador = if jD >0 then  show jD else []
comprimeDisparos (DisparoChoque {jogadorDisparo = jD , tempoDisparo = tD}:t) 
                  = "c" ++ jogador ++ "t" ++ show tD ++ comprimeDisparos t
                  where jogador = if jD >0 then  show jD else []
comprimeDisparos _ = []


-- * Jogadores


-- | Comprime os jogadores em formato textual.
comprimeJogadores :: [Jogador] -> String
comprimeJogadores (Jogador{posicaoJogador = pJ, direcaoJogador = dJ, vidasJogador = vJ, lasersJogador = lJ, choquesJogador = cJ}:t) 
                    = (show dJ ++ posicaoString pJ ++ vidas  ++ lasers  ++ choques) ++  comprimeJogadores t
                  where vidas = if vJ >0 then "v" ++ show vJ else []
                        lasers = if lJ >0 then "n" ++ show lJ else []
                        choques = if cJ>0 then "b" ++ show cJ else []
comprimeJogadores _ = []


-- | Passa uma Posicao para uma String
posicaoString :: PosicaoGrelha -> String
posicaoString (x,y) = show x ++ "," ++ show y


-- | Comprime o Mapa em formato textual.
comprimeMapa :: Mapa -> String
comprimeMapa [] = []
comprimeMapa m = dimensoesMapa m ++ if not(null mapa) then mapa else []
            where mapa = tupleToString(ajuntaMapa (group(comprimeBlocos m m (0,0))))

-- | Dá as dimensões do Mapa
dimensoesMapa :: Mapa -> String
dimensoesMapa [[]] = "0,0"
dimensoesMapa h = if linhas == colunas then show linhas  else show linhas ++ "," ++ show colunas
                  where linhas = length h
                        colunas = length (head h)

-- | Passa [(Int,Char)] para uma String 
tupleToString :: [(Int,Char)] -> String
tupleToString [] = []
tupleToString [(x,y)] = if y == '_' then [] else y:show x
tupleToString ((x,y):t) = if x== 1 then y:tupleToString t else [y] ++ show x ++ tupleToString t

-- |Condensa a informação do mapa quando há tipos de parede repetidos
ajuntaMapa :: [String] -> [(Int,Char)]
ajuntaMapa  =  map (\ x -> (length x, head x)) 

-- |Comprime os Tipos de Parede do Mapa numa String
comprimeBlocos :: Mapa -> Mapa -> Posicao -> String
comprimeBlocos [] _ _ = []
comprimeBlocos ([]:t) m pos = comprimeBlocos t m pos
comprimeBlocos ((Bloco Indestrutivel:xs):y) m (px,py) = if eBordaMatriz (px,py) m then comprimeBlocos(xs:y) m pos else '#':comprimeBlocos (xs:y) m pos
                                                        where pos = if py+1 < length(head m) then (px,py+1) else (px+1,0)
comprimeBlocos ((Bloco Destrutivel:xs):y) m (px,py) = '|':comprimeBlocos (xs:y) m pos
                                                        where pos = if py+1 < length(head m) then (px,py+1) else (px+1,0)                                                        
comprimeBlocos ((Vazia:xs):y) m (px,py) = '_':comprimeBlocos (xs:y) m pos
                                                        where pos = if py+1 < length(head m) then (px,py+1) else (px+1,0)

-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
descomprime :: String -> Estado
descomprime s = Estado { mapaEstado = descomprimeMapa mapa (descomprimeDimensao dimStr) , jogadoresEstado = descomprimeJogadores jogadores 0 , disparosEstado = descomprimeDisparos disparos 0}
                where jogadores = descodificaJogadores s False
                      disparos = descodificaDisparos s False
                      dimStr = descodificaDimensao s
                      mapa = drop (length dimStr) (take (length s - (length jogadores + length disparos))  s)

-- | Descodifica os Disparos 
-- | Descodifica os Disparos
descodificaDisparos :: String  -> Bool -> String
descodificaDisparos [] _ = []
descodificaDisparos ('c':t) False = 'c':descodificaDisparos t True
descodificaDisparos ('a':t) False = 'a':descodificaDisparos t True
descodificaDisparos ('l':t) False = 'l':descodificaDisparos t True
descodificaDisparos (_:t) False = descodificaDisparos t False
descodificaDisparos (h:t) True = h:descodificaDisparos t True       


-- | Descodifica os Jogadores
descodificaJogadores :: String -> Bool -> String
descodificaJogadores [] _ = []
descodificaJogadores ('c':_) True = []
descodificaJogadores ('a':_) True = []
descodificaJogadores ('l':_) True = []
descodificaJogadores (h:t) False = if headIgualCarater h "DBCE" then h:descodificaJogadores t True else descodificaJogadores t False
descodificaJogadores (h:t) True = if headIgualCarater h "cal" then [] else h:descodificaJogadores t True

-- | Verifica se a cabeça é igual ao caracter
headIgualCarater :: Char -> String -> Bool
headIgualCarater _ [] = False
headIgualCarater x (h:t) = (x == h) || headIgualCarater x t

-- | Descomprime a Dimensao do Mapa de forma a construir o Mapa
descodificaDimensao :: String  -> String
descodificaDimensao [] = []
descodificaDimensao (h:t) = if headIgualCarater h "_#|BDCEcal" then [] else h:descodificaDimensao t

-- | Recebe a parte da String relativamente à parte do Mapa e descomprime o Mapa
descomprimeMapa :: String -> Dimensao -> Mapa
descomprimeMapa [] d = mapaInicial d
descomprimeMapa _ (0,0) = [[]]
descomprimeMapa mapaCoded d = atualizaMapaComEstado mapaCoded (0,0)  (mapaInicial d) 

-- | Descomprime Dimensao
descomprimeDimensao :: String -> Dimensao
descomprimeDimensao s = if quadrada then (linhas,linhas) else (linhas,colunas)
                        where pstring = stringAteChar s ','
                              linhas = read pstring::Int
                              colunas = if quadrada then 0 else read(drop (length pstring + 1) s)::Int
                              quadrada =  length pstring == length s

-- | Pega na String e constroi o mapa
atualizaMapaComEstado :: String -> Posicao  -> Mapa -> Mapa
atualizaMapaComEstado [] _ m = m
atualizaMapaComEstado str p m = atualizaMapaComEstado res pos mapa
                        where (bl,n,res) = trabalhaString str
                              (mapa,pos) = atualizaMapaPorChar (bl,n) p m
                              
-- | Trabalha a string de maneira a dar os caracteres para atualizar o mapa e o resto da string
trabalhaString :: String -> (Char,Int,String)
trabalhaString s = (head s,rep,str) 
                  where rep = if not(null digStr) then read digStr::Int else 1
                        digStr = takeWhile isDigit (tail s)
                        str = drop (1+length digStr) s

-- | Atualiza o Mapa com os tipos de parede do Estado descomprimido
atualizaMapaPorChar:: (Char,Int) -> Posicao  ->Mapa  -> (Mapa,Posicao)
atualizaMapaPorChar (_, 0) pos m  = (m,pos)
atualizaMapaPorChar ('_',n) (px,py) m = if eBordaMatriz (px, py) m then atualizaMapaPorChar('_',n) pos m  else atualizaMapaPorChar('_',n-1) pos (atualizaPosicaoMatriz (px,py) Vazia m) 
                                        where pos = if py+1 < length(head m) then (px,py+1) else (px+1,0)
atualizaMapaPorChar ('#',n) (px,py) m = if eBordaMatriz (px,py) m then atualizaMapaPorChar('#',n) pos m  else atualizaMapaPorChar('#',n-1) pos (atualizaPosicaoMatriz (px,py) (Bloco Indestrutivel) m)  
                                        where pos = if py+1 < length(head m) then (px,py+1) else (px+1,0)
atualizaMapaPorChar ('|',n) (px,py) m = if eBordaMatriz (px,py) m then atualizaMapaPorChar('|',n) pos m  else atualizaMapaPorChar('|',n-1) pos (atualizaPosicaoMatriz (px,py) (Bloco Destrutivel) m)  
                                        where pos = if py+1 < length(head m) then (px,py+1) else (px+1,0)
atualizaMapaPorChar _ pos m = (m,pos)

-- | Funcao que recebe a string relativa aos jogadores e passa a descomprime Jogador para descomprimir os jogadores
descomprimeJogadores :: String -> Int -> [Jogador]
descomprimeJogadores [] _ = []
descomprimeJogadores s x = if length player-1 > x  then descomprimeJogador (player !! (x+1)):descomprimeJogadores s (x+1) else []    
                          where player = split (keepDelimsL $oneOf "BCED") s


-- |Funcao que descomprime Jogador
descomprimeJogador :: String -> Jogador
descomprimeJogador s = Jogador{posicaoJogador = posicao,direcaoJogador =  direcao,vidasJogador = vidas,  lasersJogador = lasers, choquesJogador = choques }
                        where jogState = split (oneOf "vnb") s
                              temVidas = "v" `isInfixOf` s
                              temLasers = "n" `isInfixOf` s
                              temChoques = "b" `isInfixOf` s
                              posicao = descomprimePosicao (drop 1 (head jogState))
                              direcao = read [head (head jogState)] :: Direcao
                              vidas = if temVidas then read (stringToNumber jogState 'v') :: Int else 0
                              lasers = if temLasers then read (stringToNumber jogState 'n') :: Int else 0
                              choques = if temChoques then read (stringToNumber jogState 'b') :: Int else 0


-- | Funcao que descomprime a Posicao
descomprimePosicao :: String -> PosicaoGrelha
descomprimePosicao s = (read x::Int,read y::Int)
                  where x = stringAteChar s ','
                        y = drop (length x +1) s

-- | Passa uma string relativa ao jogador e obtem as strings com os valores das vidas , lasers , choques
stringToNumber :: [String] -> Char -> String
stringToNumber [] _ = []
stringToNumber (h:t) x = if h == [x] then head t else stringToNumber t x

-- | Descomprime a parte da string relativamente aos disparos de forma a passar a funcao descomprimeDisparo
descomprimeDisparos :: String -> Int -> [Disparo]
descomprimeDisparos [] _ = []
descomprimeDisparos s x = if length disparo-1 > x  then  descomprimeDisparo (disparo !! (x+1)):descomprimeDisparos s (x+1) else []    
                          where disparo = split (keepDelimsL $oneOf "alc") s

-- |Cria o disparo atraves da string que recebe 
descomprimeDisparo :: String ->  Disparo
descomprimeDisparo (h:t) = criaDisparoCompresso (getArma h ,strJogador) strPosicao direcao ticks
                                where str = split (keepDelimsL $oneOf "BCDEt") t
                                      strJogador = if not (null (head str)) then read (head str) :: Int else 0
                                      strPosicao = if not isChoque then descomprimePosicao (tail (str!!1)) else (0,0)
                                      direcao = if not isChoque then read [head (str !! 1)] :: Direcao else B
                                      isChoque = h == 'c'
                                      ticks = if isChoque then read (tail (str!!1))::Ticks  else 0


-- | Recebe um char e retorna uma arma                                      
getArma :: Char -> Arma
getArma 'a' = Canhao
getArma 'l' = Laser
getArma 'c' = Choque
getArma _ = Canhao              


-- | Cria disparo da descompressao da String relativa a um disparo
criaDisparoCompresso :: (Arma,Int)->PosicaoGrelha -> Direcao -> Ticks -> Disparo
criaDisparoCompresso (Canhao,njog) pos dir _ = DisparoCanhao njog pos dir
criaDisparoCompresso (Laser,njog) pos dir _ = DisparoLaser njog pos dir
criaDisparoCompresso (Choque,njog) _ _ t = DisparoChoque njog t

-- | Retorna a string até encontrar o char
stringAteChar :: String-> Char -> String
stringAteChar [] _ = []
stringAteChar (h:t) c = if h == c then [] else h:stringAteChar t c

