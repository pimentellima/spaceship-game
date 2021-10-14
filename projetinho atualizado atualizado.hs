{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

main = do -- (not (encontroRetas2 (-5,9) (-2,5) [(-6,2), (-6,7), (-5,7), (-5,2)]))
          activityOf ((0,0),0, (0,0)) update visualization
         
          

type World = (Angulo, Velocidade, Posicao)
type Posicao = (Double, Double)
type Angulo = (Double, Double)
type Velocidade = Double
type Reta = (Ponto, Ponto)
type Ponto = (Double, Double)
type Coeficientes = (Double, Double)
type Poligono = [Ponto]

-- Activities 1 e 2: Mundo redondo com obstáculos em que a nave é controlada pelo jogador e pode rebater ou ser destruída. A direção da nave é controlada pelas teclas A e S e sua aceleração pela tecla W. A nave acelera até três estágios de velocidade, e ao soltar o comando do acelerador, a nave desacelera.

visualization :: World -> Picture
visualization ((a,b),v, (x,y)) 
         | x >= 7 && x < 7.25 && y <= 5 && y >= -5 = pictures [styledLettering Plain SansSerif "Nave destruida. Aperte Enter para reiniciar.", colored brown (thickPolyline 0.5 [(-7,-5),(-7,5)]), 
 colored yellow (thickPolyline 0.5 [(7,-5),(7,5)])]
         | otherwise = pictures [ (translated x y (rotated (a * pi/360) nave)), colored brown (thickPolyline 0.5 [(-7,-5),(-7,5)]), 
 colored yellow (thickPolyline 0.5 [(7,-5),(7,5)])]
   where nave = (solidPolygon [((-0.5), 0), (0,2), (0.5, 0)])


update :: Event -> World -> World
update (KeyPress "Enter") ((a,b),v, (x,y)) = ((0,0), 0, (0,0))
update _ (a,v, (x,y)) 
         | x >= 7 && x < 7.3 && y <= 5 && y >= -5 = (a,v, (x,y))   
         | y >= 10 = (a,v, (x, -y + 0.3))
         | y <= -10 = (a,v, (x, -y - 0.3))
         | x >= 10 = (a, v, (-x + 0.3, y))
         | x <= -10 = (a, v, (-x - 0.3, y))
update (TimePassing t) ((a,b),v, (x,y)) 
                                  | novoX <= -7 && novoX > -7.25 && novoY >= -5 && novoY <= 5 = ((-a,-b),v * 0.8, (x,y))
                                  | v <= 0 = ((a,b),0, (x,y)) 
                                  | otherwise = ((a,b),v - t, (novoX, novoY))
                                       where novoX = x - sin (b * pi/360) * v * t
                                             novoY = y + cos (b * pi/360) * v * t         
update (KeyPress "Up") ((a,b),v, (x,y)) 
         | v >= 3 = ((a,b), 3, (x, y))
         | otherwise = ((a,a), v + 1, (x, y))
update (KeyPress "Left") ((a,b),v, (x,y)) = ((a + 10,b), v, (x,y))
update (KeyPress "Right") ((a,b),v, (x,y)) = ((a - 10,b), v , (x,y))
update _ (a,v, (x,y)) = (a,v, (x,y))

  


encontroRetas1 :: Ponto -> Ponto -> Ponto -> Ponto -> Bool
encontroRetas1 p1 q1 p2 q2
 | ((casoGeral_1_1 > 0) && (casoGeral_1_2 < 0)) || ((casoGeral_1_1 < 0) && (casoGeral_1_2 > 0))
 && ((casoGeral_2_1 > 0) && (casoGeral_2_2 < 0)) || ((casoGeral_2_1 < 0) && (casoGeral_2_2 > 0)) = True
 | ((casoGeral_1_1 == 0) && (casoGeral_1_2 == 0)) && ((casoGeral_2_1 == 0) && (casoGeral_2_2 == 0) &&
 (projecoes)) = True
 |otherwise = False
 where
 p1q1 = vectorDifference p1 q1
 q1p2 = vectorDifference q1 p2
 q1q2 = vectorDifference q1 q2

 p2q2 = vectorDifference p2 q2
 q2p1 = vectorDifference q2 p1
 q2q1 = vectorDifference q2 q1

 casoGeral_1_1 = fst p1q1 * snd q1p2 - snd p1q1 * fst q1p2
 casoGeral_1_2 = fst p1q1 * snd q1q2 - snd p1q1 * fst q1q2

 casoGeral_2_1 = fst p2q2 * snd q2p1 - snd p2q2 * fst q2p1
 casoGeral_2_2 = fst p2q2 * snd q2q1 - snd p2q2 * fst q2q1


 projX1 = (fst p1q1, 0)
 projX2 = (fst p2q2, 0)
 projY1 = (0, snd p1q1)
 projY2 = (0, snd p2q2)

 projecoes = encontroRetas1 projX1 projX2 projY1 projY2

encontroRetas2 :: Ponto -> Ponto -> [Ponto] -> Bool
encontroRetas2 p1 q1 pqS = or[encontroRetas1 p1 q1 pp qq |
 (pp, qq) <- zip pqS pqSswap]
 where
  pqSswap = concat [(tail pqS), [head pqS]]

encontroPoligon :: [Point] -> [Point] -> Bool
encontroPoligon pqS1 pqS2 = or[encontroRetas1 pp1 qq1 pp2 qq2
                            | (pp1, qq1) <- zip pqS1 pqSswap1,
                              (pp2, qq2) <- zip pqS2 pqSswap2]
                       where pqSswap1 = concat [(tail pqS1), [head pqS1]]
                             pqSswap2 = concat [(tail pqS2), [head pqS2]]
     
areaPoligono :: Poligono -> Double
areaPoligono poligono =  abs ( ( (sum [x1 * y2 - x2 *  y1 | x<-[0..length poligono - 2],  let x1 = fst (poligono !! x)
                                                                                              x2 = fst (poligono !! ((x+1)))
                                                                                              y1 = snd (poligono !! x)
                                                                                              y2 = snd (poligono !! (x+1)) ] ) 
                        + xn * y1 - x1 * yn ) / 2) 
                                                                                          where xn = fst (poligono !! (length poligono - 1))
                                                                                                y1 = snd (poligono !! 0)
                                                                                                x1 = fst (poligono !! 0)
                                                                                                yn = snd (poligono !! (length poligono - 1))
                        

centroidePoligono :: Poligono -> Ponto
centroidePoligono poligono = (-a, -b)
                           where a = (sum [(x1 + x2) * (x1 * y2 - x2 * y1) | x<-[0..length poligono -2], let x1 = fst (poligono !! x)
                                                                                                             x2 = fst (poligono !! (x+1))
                                                                                                             y1 = snd (poligono !! x)
                                                                                                             y2 = snd (poligono !! (x+1))] 
                                    + (xn + x1) * (xn * y1 - x1 * yn)) / (6 * areaPoligono poligono)
                                                                                                        where xn = fst (poligono !! (length poligono - 1))
                                                                                                              y1 = snd (poligono !! 0)
                                                                                                              x1 = fst (poligono !! 0)
                                                                                                              yn = snd (poligono !! (length poligono - 1))
                                 b = (sum [(y1 + y2) * (x1 * y2 - x2 * y1) | x<-[0..length poligono -2],  let x1 = fst (poligono !! x)
                                                                                                              x2 = fst (poligono !! (x+1))
                                                                                                              y1 = snd (poligono !! x)
                                                                                                              y2 = snd (poligono !! (x+1))]
                                    + (yn + y1) * (xn * y1 - x1 * yn)) / (6 * areaPoligono poligono)
                                                                                                        where xn = fst (poligono !! (length poligono - 1))
                                                                                                              y1 = snd (poligono !! 0)
                                                                                                              x1 = fst (poligono !! 0)
                                                                                                              yn = snd (poligono !! (length poligono - 1))

                                                                                                            
 

                                                          