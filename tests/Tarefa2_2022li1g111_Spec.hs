module Tarefa2_2022li1g111_Spec where

import LI12223
import Tarefa2_2022li1g111
import Test.HUnit

testsT2 :: Test
testsT2 = TestList [
    TestLabel "Teste 1" test1,
    TestLabel "Teste 2" test2,
    TestLabel "Teste 3" test3
    ]

test1 = TestCase (assertEqual "Adiciona uma nova linha válida ao mapa" 
(Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Rio 0,[Nenhum,Tronco,Nenhum,Tronco])]) 
(estendeMapa (Mapa 4 ([(Relva,[Nenhum, Nenhum, Arvore, Nenhum])])) 6))
test2 = TestCase (assertEqual "Adiciona uma nova linha válida ao mapa" 
(Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Estrada 2,[Carro,Carro,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Arvore])])
(estendeMapa (Mapa 4 ([(Relva,[Nenhum, Nenhum, Arvore, Nenhum]),(Estrada 2,[Carro,Carro,Nenhum,Nenhum])])) 2))
test3 = TestCase (assertEqual "Adiciona uma nova linha válida ao mapa" 
(Mapa 3 [(Relva,[Nenhum,Nenhum,Arvore]),(Estrada 2,[Carro,Carro,Nenhum]),(Rio 3,[Tronco,Tronco,Tronco]),(Rio 0,[Nenhum,Tronco,Nenhum])])
(estendeMapa (Mapa 3 ([(Relva,[Nenhum, Nenhum, Arvore]),(Estrada 2,[Carro,Carro,Nenhum]),(Rio 3,[Tronco,Tronco,Tronco])])) 0))