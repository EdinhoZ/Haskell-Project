module Tarefa3_2022li1g111_Spec where

import LI12223
import Tarefa3_2022li1g111
import Test.HUnit

testsT3 :: Test
testsT3 = TestList [
    TestLabel "Teste 1" test1,
    TestLabel "Teste 2" test2,
    TestLabel "Teste 3" test3,
    TestLabel "Teste 4" test4,
    TestLabel "Teste 5" test5
    ]

test1 = TestCase (assertEqual "Jogador mexe-se para cima" (Jogo (Jogador(1,1)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (animaJogo (Jogo (Jogador(1,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada (-1),[Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))(Move Cima)))
test2 = TestCase (assertEqual "Jogador mexe-se para baixo" (Jogo (Jogador(1,2)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (animaJogo (Jogo (Jogador(1,1)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada (-1),[Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))(Move Baixo)))
test3 = TestCase (assertEqual "Jogador mexe-se para a direita" (Jogo (Jogador(2,2)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (animaJogo (Jogo (Jogador(1,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada (-1),[Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))(Move Direita)))
test4 = TestCase (assertEqual "Jogador mexe-se para a esquerda" (Jogo (Jogador(0,2)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (animaJogo (Jogo (Jogador(1,2)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada (-1),[Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))(Move Esquerda)))
test5 = TestCase (assertEqual "Jogador parado em cima dum Tronco" (Jogo (Jogador(0,0)) (Mapa 3 [(Rio 2,[Tronco,Nenhum,Nenhum]),(Estrada (-1),[Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])])) (animaJogo (Jogo (Jogador(1,0)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada (-1),[Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum])]))(Parado)))