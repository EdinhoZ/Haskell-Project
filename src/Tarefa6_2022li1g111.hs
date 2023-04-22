{- |
Module      : Tarefa6_2022li1g111
Description : Aplicação gráfica completa
Copyright   : Edgar Alexandre Capela Pinto <a104081@alunos.uminho.pt>
              Diogo Alexandre Pereira Da Costa <a95147@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}

module Tarefa6_2022li1g111 where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import LI12223
import Tarefa2_2022li1g111
import Tarefa3_2022li1g111
import Tarefa4_2022li1g111
import Tarefa5_2022li1g111

import Data.Maybe

type Textura = [(String, Picture)]

type Skins = [(String,Picture)]

type Obstaculos = [(String,Picture)]

type Tempo = Float

type Pontuacao = Int

type EstadoGloss = (Menu, Jogo, Textura, Skins, Obstaculos, Tempo, Pontuacao)

l1 :: Float
l1 = 137

l2 :: Float
l2 = 135
 
compr :: Float
compr = -890

altura :: Float
altura = 473

{-|
Esta função e a seguinte permitem-nos saber as coordenadas do Jogador na parte gráfica do jogo.
-}
realPlayerX :: Int -- ^ Recebe a coordenada no código
 -> Float -- ^ E converte para podermos aplicar na parte gráfica
realPlayerX x = fromIntegral x *l1 +compr

realPlayerY :: Int -- ^ Recebe a coordenada no código
 -> Float -- ^ E converte para podermos aplicar na parte gráfica
realPlayerY y = -fromIntegral y *l2 +altura

{-|
Esta função serve para mostrar como vai ser o início do jogo (onde vai estar o jogador, a localização dos obstáculos e quais terrenos surgem no início).
-}
estadoInicial :: Textura -- ^ Recebe as texturas dos terrenos, obstáculos e jogador
 -> Skins -- ^ O vestuário do jogador
 -> Obstaculos -- ^ Os obstáculos gráficos
 -> EstadoGloss -- ^ E devolve como vai ser o início do jogo na parte gráfica
estadoInicial textura skins obstaculos = (Opcoes Jogar, (Jogo (Jogador (7,7) Galinha) (Mapa 8 [(Rio 2,[Nenhum,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Tronco,Nenhum]),(Estrada (-1),[Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum]), (Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore]), (Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum]), (Rio (-2),[Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Tronco]), (Estrada 1,[Nenhum,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro]), (Relva, [Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum])])), textura, skins, obstaculos, 0, 0)

{-|
Esta função e as 2 seguintes servem para converter os terrenos, os obstáculos e o jogador em strings para podermos aplicar no IO para o programa mostrar todos os elementos do jogo
-}
conversorTe :: Terreno -- ^ Recebe o terreno
 -> String -- ^ E transforma numa string
conversorTe terreno = case terreno of 
                                  (Rio _) -> "Rio"
                                  (Estrada _) -> "Estrada"
                                  Relva -> "Relva"

conversorObs :: Obstaculo -> String
conversorObs obstaculo = case obstaculo of 
                                        Nenhum -> "Nenhum"
                                        Tronco -> "Tronco"
                                        Arvore -> "Arvore"
                                        Carro -> "Carro"

conversorJogador :: TexturaPersonagem -> String
conversorJogador g = case g of 
                            Galinha -> "Galinha"

getList :: Int -> [a] -> a
getList n l = (!!) l n 

getMapa (Jogo j m) = m

getJogador (Jogo j m) = j

{-|
Esta função permite ao utilizador, após dar um input, realizar uma determinada ação dentro do jogo.
-}
reagirEstadoGloss :: Event -- ^ Recebe o input
 -> EstadoGloss -- ^ E o estado em que o jogo se encontra
 -> EstadoGloss -- ^ E devolve o estado do jogo após ser introduzido o input
reagirEstadoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, jogo, textura, skins, obstaculos, t, p) = (ModoJogo, jogo, textura, skins, obstaculos, t, p)
reagirEstadoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, jogo, textura, skins, obstaculos, t, p) = (Opcoes Sair, jogo, textura, skins, obstaculos, t, p)
reagirEstadoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, jogo, textura, skins, obstaculos, t, p) = (Opcoes Sair, jogo, textura, skins, obstaculos, t, p)
reagirEstadoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo, textura, skins, obstaculos, t, p) = (Opcoes Jogar, jogo, textura, skins, obstaculos, t, p)
reagirEstadoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo, textura, skins, obstaculos, t, p) = (Opcoes Jogar, jogo, textura, skins, obstaculos, t, p)
reagirEstadoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo, textura, skins, obstaculos, t, p) = error "Fim de Jogo"
reagirEstadoGloss (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (Jogo j m), textura, skins, obstaculos, t, p) = fimJogo (ModoJogo, Jogo (movJogador j m (Move Cima)) m, textura, skins, obstaculos, t, p+1)
reagirEstadoGloss (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo j m), textura, skins, obstaculos, t, p) = fimJogo (ModoJogo, Jogo (movJogador j m (Move Baixo)) m, textura, skins, obstaculos, t, p-1)
reagirEstadoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo j m), textura, skins, obstaculos, t, p) = fimJogo (ModoJogo, Jogo (movJogador j m (Move Esquerda)) m, textura, skins, obstaculos, t, p)
reagirEstadoGloss (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo j m), textura, skins, obstaculos, t, p) = fimJogo (ModoJogo, Jogo (movJogador j m (Move Direita)) m, textura, skins, obstaculos, t, p)
reagirEstadoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Terminou, Jogo (Jogador (x,y) g) m, textura, skins, obstaculos, t, p) = estadoInicial textura skins obstaculos
reagirEstadoGloss _ w = w

{-|
Esta função faz com que seja contado tempo no jogo
-}
reagirTempoGloss :: Float -- ^ Recebe o tempo passado
 -> EstadoGloss -- ^ O estado inicial
 -> EstadoGloss -- ^ E o estado após passar o tempo
reagirTempoGloss tempo (ModoJogo, (Jogo j m), textura, skins, obstaculos, t, p)
    | t  > 1 = fimJogo (ModoJogo, deslizaMapa 0 (Jogo j (movObs m)), textura, skins, obstaculos, 0, p)
    | otherwise = fimJogo (ModoJogo, Jogo j m, textura, skins, obstaculos, tempo+t, p)
reagirTempoGloss tempo (m, j, te, s, ob, t, p) = (m, j, te, s, ob, t, p)

{-|
Esta função serve para determinar o fim do jogo
-}
fimJogo :: EstadoGloss -- ^ Recebe o estado do jogo atual
 -> EstadoGloss -- ^ E devolve o estado do jogo caso tenha terminado
fimJogo (ModoJogo, Jogo j m, textura, skins, obstaculos, t, p)
    | jogoTerminou (Jogo j m) = (Terminou, Jogo j m, textura, skins, obstaculos, t, p)
    | otherwise = (ModoJogo, Jogo j m, textura, skins, obstaculos, t, p)

{-| 
Estas funções que se seguem são as funções que permitem que todos os elementos do jogo sejam transformados em elementos gráficos.
Estas funções têm todas o mesmo fundamento, alterando apenas os elementos que a função transforma.
-}
desenharTerreno :: Float -- ^ Recebe coordenada x do terreno
 -> Float -- ^ E coordenada y
 -> Terreno -- ^ Recebe um terreno
 -> Textura -- ^ A textura do determinado terreno
 -> Picture -- ^ E transforma num terreno gráfico
desenharTerreno x y te textura = Translate x y textura1
    where textura1 = (fromJust . lookup (conversorTe te)) textura

desenharLinha :: Float -> Float -> (Terreno, [Obstaculo]) -> Textura -> [Picture]
desenharLinha x y (te, (h:t)) textura = terreno : restante 
    where terreno = desenharTerreno x y te textura 
          restante = desenharLinha (x+l1) y (te, t) textura
desenharLinha _ _ _ _ = []

desenharMapa :: Float -> Float -> Mapa -> Textura -> [Picture]
desenharMapa x y (Mapa l (h:t)) textura = linha ++ restante
    where linha = desenharLinha x y h textura
          restante = desenharMapa x (y-l2) (Mapa l t) textura
desenharMapa _ _ _ _ = [] 

desenharObstaculo :: Float -> Float -> Obstaculo -> Obstaculos -> Picture
desenharObstaculo x y obs ob = Translate x y ob1
    where ob1 = (fromJust . lookup (conversorObs obs)) ob

desenharObstaculos :: Float -> Float -> (Terreno, [Obstaculo]) -> Obstaculos -> [Picture]
desenharObstaculos x y (te, (h:t)) ob = obstaculo : restante 
    where obstaculo = desenharObstaculo x y h ob
          restante = desenharObstaculos (x+l1) y (te, t) ob
desenharObstaculos _ _ _ _ = [] 

desenharObstaculosNoMapa :: Float -> Float -> Mapa -> Obstaculos -> [Picture]
desenharObstaculosNoMapa x y (Mapa l (h:t)) ob = obstaculos ++ restante
    where obstaculos = desenharObstaculos x y h ob
          restante = desenharObstaculosNoMapa x (y-l2) (Mapa l t) ob
desenharObstaculosNoMapa _ _ _ _ = []

desenharJogador :: Int -> Int -> TexturaPersonagem -> Skins -> Picture
desenharJogador x y g skins = Translate (realPlayerX x) (realPlayerY y) imagem
    where imagem = (fromJust . lookup (conversorJogador g)) skins

desenharJogadores :: [Jogador] -> Skins -> [Picture]
desenharJogadores ((Jogador (x,y) g):t) skins = (desenharJogador x y g skins) : (desenharJogadores t skins)
desenharJogadores [] _ = []

desenharOpcao :: String -> Picture
desenharOpcao opcao = Translate (-50) 0 $ Scale (0.5) (0.5) $ Text opcao

desenharEstadoGloss :: EstadoGloss -> Picture
desenharEstadoGloss (Opcoes Jogar, (Jogo j m), textura, skins, obstaculos, t, p) = Pictures [Color blue $ desenharOpcao "Jogar", Translate 0 (-70) $ desenharOpcao "Sair"]
desenharEstadoGloss (Opcoes Sair, (Jogo j m), textura, skins, obstaculos, t, p) = Pictures [desenharOpcao "Jogar", Color blue $ Translate 0 (-70) $ desenharOpcao "Sair"]
desenharEstadoGloss (Terminou, (Jogo j m), textura, skins, obstaculos, t, p) = Pictures [Translate (-200) 70 $ Color blue $ desenharOpcao "GameOver", Translate (-400) (-70) $ Color blue $ desenharOpcao ("A sua pontuacao foi de "++show p)]
desenharEstadoGloss (ModoJogo, (Jogo j m), textura, skins, obstaculos, t, p) = Pictures (desenharMapa (-890) 473 mapa textura ++ desenharObstaculosNoMapa (-890) 473 mapa obstaculos ++ desenharJogadores jogador skins) 
    where mapa = getMapa (Jogo j m)
          jogador = [getJogador (Jogo j m)]

{-|
Esta função dá-nos a framerate do jogo.
-}
fr :: Int 
fr = 60 

{-|
Esta função diz como o jogo vai ser mostrado no ecrã.
-}
fs :: Display
fs = FullScreen
