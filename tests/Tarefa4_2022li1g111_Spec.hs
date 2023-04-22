module Tarefa4_2022li1g111_Spec where

import LI12223
import Tarefa4_2022li1g111
import Test.HUnit

testsT4 :: Test
testsT4 = TestList [
    TestLabel "Teste 1" test1,
    TestLabel "Teste 2" test2,
    TestLabel "Teste 3" test3,
    TestLabel "Teste 4" test4
    ]

test1 = TestCase (assertEqual "Jogo termina porque jogador fica fora do mapa" True (jogoTerminou (Jogo (Jogador (1,4)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 2,[Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum])]))))
test2 = TestCase (assertEqual "Jogo termina porque jogador foi atropelado" True (jogoTerminou (Jogo (Jogador (0,1)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 2,[Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum])]))))
test3 = TestCase (assertEqual "Jogo termina porque jogador afogou-se" True (jogoTerminou (Jogo (Jogador (0,0)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 2,[Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum])])))) 
test4 = TestCase (assertEqual "Jogador está no rio em cima dum Tronco(Jogo não acaba)" False (jogoTerminou (Jogo (Jogador (1,0)) (Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum]),(Estrada 2,[Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum])]))))