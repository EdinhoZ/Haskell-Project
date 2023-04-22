{- |
Module      : Tarefa5_2022li1g111
Description : Deslize do mapa
Copyright   : Edgar Alexandre Capela Pinto <a104081@alunos.uminho.pt>
              Diogo Alexandre Pereira Da Costa <a95147@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}

module Tarefa5_2022li1g111 where

import LI12223
import Tarefa2_2022li1g111

{-|
Esta função serve para, ao criar uma nova linha no mapa, a última linha desaparecer.
-}
deslizaMapa :: Int 
 -> Jogo -- ^ O jogo
 -> Jogo -- ^ O jogo depois do deslize do Mapa
deslizaMapa n (Jogo (Jogador (x,y) g) (Mapa l ((t,o:os):xs))) = Jogo (Jogador (x,y+1) g) (estendeMapa (Mapa l ((t,o:os):xs)) n) 