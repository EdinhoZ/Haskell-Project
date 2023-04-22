{- |
Module      : Tarefa3_2022li1g111
Description : Movimentação do personagem e obstáculos
Copyright   : Edgar Alexandre Capela Pinto <a104081@alunos.uminho.pt>
              Diogo Alexandre Pereira Da Costa <a95147@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g111 where

import LI12223
import Tarefa4_2022li1g111

{-|
Esta função permite a animação do jogo, ou seja, permite o movimento do Jogador e dos Obstáculos.
-}
animaJogo :: Jogo -- ^ Recebe o Jogo, composto pelo Jogador e pelo Mapa.
 -> Jogada -- ^ Recebe uma jogada n que permite o personagem se movimentar.
 -> Jogo -- ^ E devolve o Jogo com a movimentação do personagem e dos obstáculos.
animaJogo (Jogo j m) n = Jogo (movJogador j m n) (movObs m)

{-|
Esta função permite determinar o movimento dum obstáculo num determinado terreno.
Para tal utilizamos uma função auxiliar que permite determinar quantos espaços o obstáculo se moverá.
-}
movObs :: Mapa -- ^ Recebe o Mapa
 -> Mapa -- ^ E devolve o Mapa com os movimentos dos obstáculos
movObs (Mapa l x) = Mapa l (movObsAux x)

{-|
Esta função determina quantos espaços o obstáculo se moverá, dependendo da velocidade v de cada terreno.
-}
movObsAux :: [(Terreno,[Obstaculo])] -- ^ Recebe uma lista de pares de Terrenos com os seus Obstáculos.
 -> [(Terreno,[Obstaculo])] -- ^ Devolve essa lista depois dos movimentos dos obstáculos.
movObsAux [] = []
movObsAux ((Rio v,x):t) 
  |v<=0 = (Rio v,((drop (-v) x) ++ (take (-v) x))) : movObsAux t
  |otherwise = (Rio v,((drop ((length x)-v) x)++(take ((length x)- v) x))) : movObsAux t
movObsAux ((Estrada v,x):t) 
  |v<=0 = (Estrada v,((drop (-v) x) ++ (take (-v) x))) : movObsAux t  
  |otherwise = (Estrada v,((drop ((length x)-v) x)++(take ((length x)- v) x))) : movObsAux t
movObsAux ((Relva,x):t) = (Relva,x) : movObsAux t

movCarroAtropelamento :: Coordenadas -> Mapa -> Mapa
movCarroAtropelamento (x,y) (Mapa lm ((Estrada v,lo):t)) = if atropelamento (x,y) (Mapa lm ((Estrada v,lo):t)) 
                                                              then (Mapa lm ((Estrada v,lo):t))
                                                              else movObs (Mapa lm ((Estrada v,lo):t))

{-|
Esta função permite a movimentação do Jogador pelo Mapa depois de feita a jogada
-}
movJogador :: Jogador -- ^ A função recebe o Jogador.
 -> Mapa -- ^ Recebe o mapa.
 -> Jogada -- ^ A jogada que se pretende fazer.
 -> Jogador -- ^ E devolve o jogador depois de fazer a jogada.
movJogador (Jogador (x,y) g) (Mapa l ((te,(o:xs)):ys)) (Parado) 
  | elem (x,y) (posTroncoJogador (Mapa l ((te,(o:xs)):ys)) 0 1) = Jogador (posTroncoMov (x,y) (Mapa l ((te,(o:xs)):ys))) g  
  | otherwise = Jogador (x,y) g
movJogador (Jogador (x,y) g) (Mapa l ((te,(o:xs)):ys)) (Move n) 
  |n == Direita && elem (x+1,y) (posArvoreJogador (Mapa l ((te,(o:xs)):ys)) 0 0) = Jogador (x,y) g
  |n == Esquerda && elem (x-1,y) (posArvoreJogador (Mapa l ((te,(o:xs)):ys)) 0 0) = Jogador (x,y) g
  |n == Baixo && elem (x,y+1) (posArvoreJogador (Mapa l ((te,(o:xs)):ys)) 0 0) = Jogador (x,y) g
  |n == Cima && elem (x,y-1) (posArvoreJogador (Mapa l ((te,(o:xs)):ys)) 0 0) = Jogador (x,y) g
  |n == Direita && x == (length (o:xs))-1 = Jogador (x,y) g
  |n == Esquerda && x == 0 = Jogador (x,y) g
  |n == Baixo && y == l-1 = Jogador (x,y) g
  |n == Cima && y == 0 = Jogador (x,y) g
  |n == Cima = Jogador (x,y-1) g
  |n == Baixo = Jogador (x,y+1) g
  |n == Esquerda = Jogador (x-1,y) g
  |otherwise = Jogador (x+1,y) g
  
{-|
Esta função determina as coordenadas dum Tronco.
-}
posTroncoJogador :: Mapa -- ^ A função recebe o mapa
 -> Int -- ^ Recebe uma coordenada x
 -> Int -- ^ Recebe uma coordenada y
 -> [Coordenadas] -- ^ Devolve uma lista com as coordenadas para verificar na função movJogador
posTroncoJogador (Mapa l []) x y = []
posTroncoJogador (Mapa l ((Rio v,o):t)) x y  = posTroncoJogadorAux (Rio v,o) x y ++ posTroncoJogador (Mapa l t) x (y+1)
posTroncoJogador (Mapa l ((Estrada v,o):t)) x y = posTroncoJogador (Mapa l t) x (y+1)
posTroncoJogador (Mapa l ((Relva,o):t)) x y = posTroncoJogador (Mapa l t) x (y+1)

{-|
Função auxiliar para determinar as coordenadas dum Tronco.
-}
posTroncoJogadorAux :: (Terreno,[Obstaculo]) -> Int -> Int -> [Coordenadas]
posTroncoJogadorAux (Rio v,[]) x y = []
posTroncoJogadorAux (Rio v,(h:t1)) x y
  |h == Tronco = (x,y) : posTroncoJogadorAux (Rio v,t1) (x+1) y  
  |otherwise = posTroncoJogadorAux (Rio v,t1) (x+1) y

{-|
Esta função determina as coordenadas do jogador se ele estiver parado em cima dum Tronco que se move.
-}
posJogadorParadoTronco :: Coordenadas -- ^ Recebe as coordenadas do jogador
 -> Mapa -- ^ Recebe o mapa
 -> Coordenadas -- ^ E devolve as coordenadas do jogador depois do movimento do Tronco
posJogadorParadoTronco (x,y) (Mapa l ((Rio v,o):t)) 
  | x + v < 0 = (x+(l-v),y)
  | otherwise = (x+v,y)  

{-|
Esta função serve para verificar em que terreno se deve aplicar a função posJogadorParadoTronco.
-}
posTroncoMov :: Coordenadas -- ^ A função recebe as coordenadas do jogador
 -> Mapa -- ^ Recebe o Mapa
 -> Coordenadas -- ^ E devolve as coordenadas depois de aplicada a função a cada um dos terrenos.
posTroncoMov (x,y) (Mapa l []) = (x,y)
posTroncoMov (x,y) (Mapa l ((Rio v,o):t)) 
  | elem (x,y) (posTroncoJogador (Mapa l [(Rio v,o)]) 0 1) = posJogadorParadoTronco (x,y) (Mapa l [(Rio v,o)]) 
  | otherwise = posTroncoMov (x,y) (Mapa l t) 
posTroncoMov (x,y) (Mapa l ((Estrada v,o):t)) = posTroncoMov (x,y) (Mapa l t)
posTroncoMov (x,y) (Mapa l ((Relva,o):t)) = posTroncoMov (x,y) (Mapa l t)     

{-|
Estas funções tem o intuito de verificar se o jogador depois de se mover não fica no mesmo espaço que uma Árvore.
A função posArvoreJogador determina as coordenadas duma Arvore no Mapa e a função posArvoreJogadorAux determina se está nas mesmas coordenadas do Jogador.
-}
posArvoreJogador :: Mapa -- ^ A função recebe o mapa
 -> Int -- ^ Uma coordenada x
 -> Int -- ^ Uma coordenada y
 -> [Coordenadas] -- ^ E devolve uma lista de coordenadas para verificar na função movJogador
posArvoreJogador (Mapa l []) x y = []
posArvoreJogador (Mapa l ((Relva,o):t)) x y  = posArvoreJogadorAux (Relva,o) x y ++ posArvoreJogador (Mapa l t) x (y+1)
posArvoreJogador (Mapa l ((Estrada v,o):t)) x y = posArvoreJogador (Mapa l t) x (y+1)
posArvoreJogador (Mapa l ((Rio v,o):t)) x y = posArvoreJogador (Mapa l t) x (y+1)

{-|
Função auxiliar para verificar se o jogador não fica no mesmo espaço duma Árvore 
-}
posArvoreJogadorAux :: (Terreno,[Obstaculo]) -- ^ A função recebe um par dum Terreno com os seus Obstáculos.
 -> Int -- ^ Uma coordenada x
 -> Int -- ^ Uma coordenada y
 -> [Coordenadas] -- ^ E devolve uma lista de coordenadas para verificar na função posArvoreJogador
posArvoreJogadorAux (Relva ,[]) x y = []
posArvoreJogadorAux (Relva ,(h:t1)) x y
  |h == Arvore = (x,y) : posArvoreJogadorAux (Relva,t1) (x+1) y  
  |otherwise = posArvoreJogadorAux (Relva,t1) (x+1) y