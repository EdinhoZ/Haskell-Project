{- |
Module      : Tarefa1_2022li1g111
Description : Validação de um mapa
Copyright   : Edgar Alexandre Capela Pinto <a104081@alunos.uminho.pt>
              Diogo Alexandre Pereira Da Costa <a95147@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g111 where

import LI12223

{- |
Esta função verifica se o mapa fornecido é um mapa válido.
-}
mapaValido :: Mapa -- ^ A função recebe um Mapa composto pela largura e uma lista de pares com os terrenos e obstáculos
 -> Bool -- ^ E confirma se o mapa é válido
mapaValido (Mapa l []) = True
mapaValido (Mapa l ((p,(x:y)):t)) = obsTerrenos (Mapa l ((p,(x:y)):t)) && velocidadeTerrenos (Mapa l ((p,(x:y)):t)) && tamanhoObs (Mapa l ((p,(x:y)):t)) && recebeTerreno (Mapa l ((p,(x:y)):t)) && matchTamanhos (Mapa l ((p,(x:y)):t)) && repTerrenos (Mapa l ((p,(x:y)):t))

{- |
Esta função verifica se não encontramos obstáculos em terrenos que não é suposto.
-}
obsTerrenos :: Mapa -- ^ Tal como a anterior, esta recebe um mapa
 -> Bool -- ^ E verifica se os obstáculos estão nos terrenos corretos
obsTerrenos (Mapa l []) = True
obsTerrenos (Mapa l ((Relva,(x:y)):t)) | (elem Tronco (x:y) || elem Carro (x:y)) = False
                                       | otherwise = obsTerrenos (Mapa l t)
obsTerrenos (Mapa l (((Rio v),(x:y)):t)) | (elem Carro (x:y) || elem Arvore (x:y)) = False
                                         | otherwise = obsTerrenos (Mapa l t)
obsTerrenos (Mapa l (((Estrada v),(x:y)):t))| (elem Arvore (x:y) || elem Tronco (x:y)) = False
                                            | otherwise = obsTerrenos (Mapa l t)

{- |
Esta função verifica se as velocidades fornecidas aos terrenos são válidas
-}
velocidadeTerrenos :: Mapa -- ^ Esta função recebe um mapa
 -> Bool -- ^ E verifica se as velocidades estão corretas
velocidadeTerrenos (Mapa l []) = True
velocidadeTerrenos (Mapa l ((Estrada v,(x:y)):t)) = velocidadeTerrenos (Mapa l t)
velocidadeTerrenos (Mapa l ((Relva,(x:y)):t)) = velocidadeTerrenos (Mapa l t)
velocidadeTerrenos (Mapa l ((Rio v,(x:y)):(Relva,(x1:y1)):t)) = velocidadeTerrenos (Mapa l t)
velocidadeTerrenos (Mapa l ((Rio v,(x:y)):(Estrada v1,(x1:y1)):t)) = velocidadeTerrenos (Mapa l t)
velocidadeTerrenos (Mapa l ((Rio v,(x:y)):[])) = True
velocidadeTerrenos (Mapa l ((Rio v,(x:y)):(Rio v1,(x1:y1)):t)) | (v>0 && v1>0) || (v<0) && (v1<0) = False
                                                               | otherwise = velocidadeTerrenos (Mapa l ((Rio v1,(x1:y1)):t))

{- |
Esta função verifica se os tamanhos dos obstáculos são válidos, seguida de 2 auxiliares para verificar o tamanho dos Troncos e dos Carros
-}
tamanhoObs :: Mapa -- ^ A função recebe o mapa
 -> Bool -- ^ E verifica se os tamanhos dos obstáculos são válidos
tamanhoObs (Mapa l []) = True
tamanhoObs (Mapa l ((Relva, (x:y)):t)) = tamanhoObs (Mapa l t)
tamanhoObs (Mapa l ((Rio v,(x:y)):t)) | tamanhoTroncos (Mapa l ((Rio v,(x:y)): t)) == True = mapaValido (Mapa l t)
                                      | otherwise = False
tamanhoObs (Mapa l ((Estrada v,(x:y)):t)) | tamanhoCarro (Mapa l ((Estrada v,(x:y)): t)) == True = mapaValido (Mapa l t)
                                          | otherwise = False

tamanhoTroncos :: Mapa -> Bool
tamanhoTroncos (Mapa l ((Rio v,[]):t)) = True
tamanhoTroncos (Mapa l ((Rio v,(x:y)): t)) | length (x:y) <= 5 = True
                                           | x == Tronco && head y == Tronco && head (tail y) == Tronco && head (tail(tail y)) == Tronco && head (tail(tail(tail y))) == Tronco && head (tail(tail(tail(tail y)))) == Tronco = False
                                           | otherwise = tamanhoTroncos (Mapa l ((Rio v,y):t))

tamanhoCarro :: Mapa -> Bool
tamanhoCarro (Mapa l ((Estrada v,[]):t)) = True
tamanhoCarro (Mapa l ((Estrada v,(x:y)):t)) | length (x:y) <= 3 = True
                                            | x == Carro && head y == Carro && head(tail y) == Carro && head(tail(tail y))== Carro = False
                                            | otherwise = tamanhoCarro (Mapa l ((Estrada v,y):t))
{-|
Esta função verifica se nos terrenos fornecidos tem sempre alguma espaço sem nenhum obstáculo.
-}
recebeTerreno :: Mapa -- ^ A função recebe o mapa
 -> Bool -- ^ E verifica a lista de osbtáculos
recebeTerreno (Mapa l []) = True
recebeTerreno (Mapa l ((p,(x:y)):t)) | elem Nenhum (x:y) == False = False
                                     | otherwise = recebeTerreno (Mapa l t)
{-|
Esta função verifica se o tamanho da lista de obstáculos não é diferente da largura do mapa
-}
matchTamanhos :: Mapa -- ^ A função recebe o mapa
 -> Bool -- ^ E verifica a igualdade
matchTamanhos (Mapa l []) = True
matchTamanhos (Mapa l ((p,(x:y)):t)) | length (x:y) /= l = False
                                     | otherwise = matchTamanhos (Mapa l t)
{-|
Por último, esta função verifica a repetição de terrenos, ou seja, se existem mais de 4 rios contíguos, mais de 5 estradas contíguas ou mais de 5 relvas contíguas
-}
repTerrenos :: Mapa -- ^ A função recebe o mapa
 -> Bool -- ^ E faz a verificação dos terrenos contíguos
repTerrenos (Mapa l []) = True
repTerrenos (Mapa l ((Rio v,(x:y)): (Rio v1,(x1:y1)) : (Rio v2,(x2:y2)) : (Rio v3,(x3:y3)) : (Rio v4,(x4:y4)) : t)) = False
repTerrenos (Mapa l ((Estrada v,(x:y)): (Estrada v1,(x1:y1)) : (Estrada v2,(x2:y2)) : (Estrada v3,(x3:y3)) : (Estrada v4,(x4:y4)) : (Estrada v5, (x5:y5)) : t)) = False
repTerrenos (Mapa l ((Relva,(x:y)): (Relva,(x1:y1)) : (Relva,(x2:y2)) : (Relva,(x3:y3)) : (Relva,(x4:y4)) : (Relva,(x5:y5)) : t)) = False
repTerrenos (Mapa l ((p,(x:y)):t)) = repTerrenos (Mapa l t)
