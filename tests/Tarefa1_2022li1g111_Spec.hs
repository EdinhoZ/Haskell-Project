module Tarefa1_2022li1g111_Spec where

import LI12223
import Tarefa1_2022li1g111
import Test.HUnit

testsT1 :: Test
testsT1 = TestList [
    TestLabel "Teste 1" test1,
    TestLabel "Teste 2" test2,
    TestLabel "Teste 3" test3
    ]

test1 = TestCase (assertEqual "Exemplo Mapa1," True (mapaValido (Mapa 4 [(Estrada 2,[Carro,Nenhum,Nenhum,Carro]),(Relva,[Arvore,Arvore,Nenhum,Nenhum]),(Rio 2,[Nenhum,Tronco,Tronco,Nenhum])])))
test2 = TestCase (assertEqual "Exemplo Mapa2(Objeto inválido num terreno)," False (mapaValido (Mapa 4 [(Estrada 2,[Carro,Nenhum,Nenhum,Carro]),(Relva,[Arvore,Arvore,Nenhum,Nenhum]),(Rio 2,[Nenhum,Arvore,Tronco,Nenhum])])))
test3 = TestCase (assertEqual "Exemplo Mapa3(Largura do mapa é diferente da length da lista de obstaculos)," False (mapaValido (Mapa 4 [(Estrada 2,[Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum]),(Rio 2,[Nenhum,Tronco,Tronco])])))