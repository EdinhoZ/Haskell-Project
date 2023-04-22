{- |
Module      : Main_2022li1g111
Description : Programa
Copyright   : Edgar Alexandre Capela Pinto <a104081@alunos.uminho.pt>
              Diogo Alexandre Pereira Da Costa <a95147@alunos.uminho.pt>

Módulo para a execução do jogo do projeto de LI1 em 2022/23.
-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import LI12223
import Tarefa1_2022li1g111
import Tarefa2_2022li1g111
import Tarefa3_2022li1g111
import Tarefa4_2022li1g111
import Tarefa5_2022li1g111
import Tarefa6_2022li1g111

import Data.Maybe

main :: IO ()
main = do
    grass     <- loadBMP "Imagens/Textura/grass.bmp"
    river     <- loadBMP "Imagens/Textura/river.bmp"
    road      <- loadBMP "Imagens/Textura/road.bmp"
    chicken   <- loadBMP "Imagens/Skins/chicken.bmp"
    tree      <- loadBMP "Imagens/Obstaculos/tree.bmp"
    log       <- loadBMP "Imagens/Obstaculos/log.bmp"
    car       <- loadBMP "Imagens/Obstaculos/car.bmp"
    nothing   <- loadBMP "Imagens/Obstaculos/nothing.bmp"
    let texturas = [("Relva", grass), ("Rio", river), ("Estrada", road)]
        jogador = [("Galinha", chicken)]
        obstaculos = [("Tronco", log), ("Arvore", tree), ("Carro", car), ("Nenhum", nothing)]
    play fs
         (greyN 0.8)
         fr
         (estadoInicial texturas jogador obstaculos)
         desenharEstadoGloss
         reagirEstadoGloss
         reagirTempoGloss
         

