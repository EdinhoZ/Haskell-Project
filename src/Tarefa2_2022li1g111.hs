{- |
Module      : Tarefa2_2022li1g111
Description : Geração contínua de um mapa
Copyright   : Edgar Alexandre Capela Pinto <a104081@alunos.uminho.pt>
              Diogo Alexandre Pereira Da Costa <a95147@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g111 where

import LI12223
import System.Random

{-|
Esta função permite adicionar n linhas ao mapa em cima.
-}

estendeMapa :: Mapa -- ^ Recebe o mapa.
 -> Int -- ^ O número para causar aleatoriedade 
 -> Mapa -- ^ Devolve o mapa com as linhas adicionadas.
estendeMapa (Mapa largura ((te, ob):t)) num = Mapa largura ((y, escolherObstaculo (Mapa largura [(y, [])]) (num + 123)) : take (length ((te, ob):t) - 1) ((te, ob):t))
                                             where y = escolherTerreno (Mapa largura ((te, ob):t)) (num + 345)

{-|
Esta função escolhe aleatoriamente um terreno para aparecer na linha seguinte.
-}
escolherTerreno :: Mapa -> Int -> Terreno
escolherTerreno mapa num = proximosTerrenosValidos mapa num !! numAleatório 0 (length (proximosTerrenosValidos mapa num) - 1) (mkStdGen num)

{-|
Esta função determina quais podem ser os próximos terrenos válidos.
Tendo como restrições o facto de não poder ter 4 rios contíguos, 5 estradas contíguas ou 5 relvas contíguas.
-}
proximosTerrenosValidos :: Mapa -- ^ Recebe o mapa
 -> Int -- ^ E recebe o número para a aleatoriedade
 -> [Terreno] -- ^ E devolve os terrenos que podem surgir depois do último
proximosTerrenosValidos (Mapa _ ((Relva,_):(Relva,_):(Relva,_):(Relva,_):(Relva,_):t)) num = [Estrada v, Rio v] where v = numAleatório (-3) 3 (mkStdGen num) 
proximosTerrenosValidos (Mapa _ ((Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):t)) num = [Rio v,Relva] where v = numAleatório (-3) 3 (mkStdGen num)
proximosTerrenosValidos (Mapa _ ((Rio _,_):(Rio _,_):(Rio _,_):(Rio _,_):t)) num = [Estrada v,Relva] where v = numAleatório (-3) 3 (mkStdGen num)
proximosTerrenosValidos (Mapa _ ((Rio speed, _):t)) num = if speed > 0 
                                                        then [Relva, Estrada v, Rio (numAleatório (-3) 0 (mkStdGen num))]
                                                        else [Relva, Estrada v, Rio (numAleatório 0 3 (mkStdGen num))]
                                                        where v = numAleatório (-3) 3 (mkStdGen num)
proximosTerrenosValidos (Mapa _ ((te, ob):t)) num = [Relva, Estrada v, Rio v] where v = numAleatório (-3) 3 (mkStdGen num)

{-|
Esta função escolhe aleatoriamente quais vão ser os obstáculos que vão aparecer na nova linha criada, dependendo do terreno.
-}
escolherObstaculo :: Mapa -> Int -> [Obstaculo]
escolherObstaculo (Mapa largura ((te, ob):t)) num | largura > length ob = escolherObstaculo (Mapa largura ((te, ob ++ [w]):t)) (num + 3)
                                                  | otherwise = ob
                                                  where w = proximosObstaculosValidos largura (te, ob) !! numAleatório 0 (length (verificarObstaculos (te, ob)) - 1) (mkStdGen num)

{-|
Esta função permite determinar quais n obstáculos se podem adicionar a um par de terreno com lista de obstáculos. (Sendo n o número de obstáculos a adicionar)
Para tal, teremos que relembrar que não podemos ter mais de 4 troncos seguidos e mais de 3 carros seguidos.
-}
proximosObstaculosValidos :: Int -- ^ A função recebe a largura do mapa
 -> (Terreno, [Obstaculo]) -- ^ O par do terreno com a lista de obstáculos
 -> [Obstaculo] -- ^ E devolve quais obstáculos podemos adicionar
proximosObstaculosValidos largura (te, l) | largura == length l = []
                                           | (largura - 1) == length l && notElem Nenhum l = [Nenhum]
                                           | otherwise = verificarObstaculos (te, l)

{-|
Esta função faz a verificação dos obstáculos contíguos.
-}
verificarObstaculos :: (Terreno, [Obstaculo]) -> [Obstaculo]
verificarObstaculos (Rio _, l) | contatroncos 0 l < 5 = [Tronco, Nenhum]
                           | otherwise = [Nenhum]
verificarObstaculos (Estrada _, l) | contacarros 0 l < 3 = [Carro, Nenhum]
                               | otherwise = [Nenhum]
verificarObstaculos (Relva, l) = [Arvore, Nenhum]

contatroncos :: Int -> [Obstaculo] -> Int
contatroncos n [] = n
contatroncos n (Tronco:t) = contatroncos (n+1) t
contatroncos n (Nenhum:t) = contatroncos 0 t

contacarros :: Int -> [Obstaculo] -> Int
contacarros n [] = n
contacarros n (Carro:t) = contacarros (n+1) t
contacarros n (Nenhum:t) = contacarros 0 t

{-|
Esta função permite-nos gerar um número aleatório para a velocidade dos terrenos e para qual terreno e obstáculos surgirão.
-}
numAleatório :: (RandomGen n) => Int -> Int -> n -> Int
numAleatório a b seed = fst (randomR (a, b) seed)