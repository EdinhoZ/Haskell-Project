{- |
Module      : Tarefa4_2022li1g111
Description : Determinar se o jogo terminou
Copyright   : Edgar Alexandre Capela Pinto <a104081@alunos.uminho.pt>
              Diogo Alexandre Pereira Da Costa <a95147@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g111 where

import LI12223
{-|
Esta função determina se o jogo terminou utilizando outras funções para tal.
-}
jogoTerminou :: Jogo -- ^ A função recebe o jogo (Jogador e Mapa)
 -> Bool -- ^ E verifica se o jogo terminou
jogoTerminou (Jogo (Jogador (x,y) g) (Mapa lm ((t,lo):t1)))  
    = (limiteDoMapa (x,y) (Mapa lm ((t,lo):t1))) || (atropelamento (x,y) (Mapa lm ((t,lo):t1))) || (afogamento (x,y) (Mapa lm ((t,lo):t1)))

{-|
Esta função verifica se o jogador não anda fora do mapa.
-}
limiteDoMapa :: Coordenadas -- ^ Recebe as coordenadas do jogador
 -> Mapa -- ^ O mapa
 -> Bool -- ^ E verifica se o jogador está fora do mapa
limiteDoMapa (x,y) (Mapa lm ((t,lo):t1))
  | (x >= length lo || x < 0) = True
  | (y >= lm || y < 0) = True
  | otherwise = False

{-|
Esta função verifica se o jogador, quando se encontra na estrada, foi atropelado, ou seja, está no mesmo sitío que um carro.
Foram utilizadas duas funções auxiliares(posCarro e auxPosCarro) para poder fazer essa verificação.
Estas funções verificam as posições dos carros.
-}
atropelamento :: Coordenadas -- ^ Recebe então as coordenadas do jogador
 -> Mapa -- ^ Recebe o mapa
 -> Bool -- ^ E verifica se o jogador foi atropelado.
atropelamento (x,y) (Mapa lm []) = False
atropelamento (x,y) (Mapa lm ((Rio v,lo):t)) = atropelamento (x,y) (Mapa lm t)
atropelamento (x,y) (Mapa lm ((Relva,lo):t)) = atropelamento (x,y) (Mapa lm t)
atropelamento (x,y) (Mapa lm ((Estrada v,lo):t))
  | (x,y) `elem` (posCarro (Mapa lm ((Estrada v,lo):t)) 0 1) = True
  | otherwise = False

posCarro :: Mapa -> Int -> Int -> [Coordenadas] 
posCarro (Mapa lm []) x y = []
posCarro (Mapa lm ((Rio v,lo):t)) x y = posCarro (Mapa lm t) x (y+1)
posCarro (Mapa lm ((Relva,lo):t)) x y = posCarro (Mapa lm t) x (y+1)
posCarro (Mapa lm ((Estrada v,lo):t)) x y = auxPosCarro (Estrada v,lo) x y ++ posCarro (Mapa lm t) x (y+1)

auxPosCarro :: (Terreno, [Obstaculo]) -> Int -> Int -> [Coordenadas]
auxPosCarro (Estrada v,[]) x y = []
auxPosCarro (Estrada v,(h:t)) x y 
    | h == Carro = (x,y) : auxPosCarro (Estrada v,t) (x+1) y
    | otherwise = auxPosCarro (Estrada v,t) (x+1) y

{-|
Esta função verifica se o jogador, quando num rio, se afogou, ou seja, não se encontra na mesma posição dum tronco.
Para tal foram usadas duas funções auxiliares (posTronco e auxPosTronco) que verificam as posições dos troncos.
-}
afogamento :: Coordenadas 
 -> Mapa -- ^ Recebe o mapa
 -> Bool -- ^ E verifica se o jogador afogou-se.  
afogamento (x,y) (Mapa lm []) = False
afogamento (x,y) (Mapa lm ((Estrada v,lo):t)) = afogamento (x,y) (Mapa lm t)
afogamento (x,y) (Mapa lm ((Relva,lo):t)) = afogamento (x,y) (Mapa lm t)
afogamento (x,y) (Mapa lm ((Rio v,lo):t)) 
  | (x,y) `elem` (semTronco (Mapa lm ((Rio v,lo):t)) 0 0) = True
  | otherwise = False

semTronco :: Mapa -> Int -> Int -> [Coordenadas] 
semTronco (Mapa lm []) x y = []
semTronco (Mapa lm ((Estrada v,lo):t)) x y = semTronco (Mapa lm t) x (y+1)
semTronco (Mapa lm ((Relva,lo):t)) x y = semTronco (Mapa lm t) x (y+1)
semTronco (Mapa lm ((Rio v,lo):t)) x y = auxSemTronco (Rio v,lo) x y ++ semTronco (Mapa lm t) x (y+1)

auxSemTronco :: (Terreno, [Obstaculo]) -> Int -> Int -> [Coordenadas]
auxSemTronco (Rio v,[]) x y = []
auxSemTronco (Rio v,(h:t)) x y 
    | h == Nenhum = (x,y) : auxSemTronco (Rio v,t) (x+1) y
    | otherwise = auxSemTronco (Rio v,t) (x+1) y