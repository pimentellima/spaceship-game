{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

main = do 
          activityOf ((0,0),0, (0,0)) update visualization
         
          

type World = (Angulo, Velocidade, Posicao)
type Posicao = (Double, Double)
type Angulo = (Double, Double)
type Velocidade = Double
type Reta = [Ponto]
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

  


pontoIntersecRetas :: Reta -> Reta -> Ponto
pontoIntersecRetas [(x0,y0),(x1,y1)] [(x2,y2),(x3,y3)] 
                                         | x0 == x1 && x2 == x3 && x0 /= x2 = error "As retas são paralelas"
                                         | x0 == x1 && x2 == x3 && x0 == x2 = error "As retas são coincidentes"
                                         | x0 == x1 && x2 /= x3 = (head [ x | x<-[x2..x3], x == x0, y<-[y0..y1], a2 * x == y ], a2 * head [ x | x<-[x2..x3], x == x0, y<-[y0..y1], a2 * x == y ] + b2)
                                         | x0 /= x1 && x2 == x3 = (head [ x | x<-[x0..x1], x == x2, y<-[y2..y3], a1 * x == y ], a1 * head [ x | x<-[x0..x1], x == x2, y<-[y2..y3], a1 * x == y ] + b1)
                                         | [] == pontoH = error "As retas não se tocam"
                                         | a1 == a2 && b1 /= b2 = error "As retas são paralelas"
                                         | a1 == a2 && b1 == b2 = error "As retas são coindicentes"
                                         | otherwise = (head pontoH, pontoV)
                                                       where a1 = fst (equacaoreta reta1)
                                                             a2 = fst (equacaoreta reta2)
                                                             b1 = snd (equacaoreta reta1)
                                                             b2 = snd (equacaoreta reta2)
                                                             reta1 =  [(x0,y0),(x1,y1)]
                                                             reta2 =  [(x2,y2),(x3,y3)]
                                                             pontoH = [x | x<-[min x0 x1..max x0 x1], x<-[min x2 x3..max x2 x3], (a1 * x) + b1 == (a2 * x) + b2]
                                                             pontoV = (a1 * head pontoH) + b1       
intersecRetasBool :: Reta -> Reta -> Bool
intersecRetasBool [(x0,y0),(x1,y1)] [(x2,y2),(x3,y3)] 
                                         | x0 == x1 && x2 == x3 && x0 /= x2 = False
                                         | x0 == x1 && x2 == x3 && x0 == x2 = False
                                         | x0 == x1 && x2 /= x3 = True
                                         | x0 /= x1 && x2 == x3 = True
                                         | [] == pontoH = False
                                         | a1 == a2 && b1 /= b2 = False
                                         | a1 == a2 && b1 == b2 = True
                                         | otherwise = True
                                                       where a1 = fst (equacaoreta reta1)
                                                             a2 = fst (equacaoreta reta2)
                                                             b1 = snd (equacaoreta reta1)
                                                             b2 = snd (equacaoreta reta2)
                                                             reta1 =  [(x0,y0),(x1,y1)]
                                                             reta2 =  [(x2,y2),(x3,y3)]
                                                             pontoH = [x | x<-[min x0 x1..max x0 x1], x<-[min x2 x3..max x2 x3], (a1 * x) + b1 == (a2 * x) + b2]
                                                             pontoV = (a1 * head pontoH) + b1     
                                                                                                    
                                                             
intersecRetaPoligonoBool :: Reta -> Poligono -> Bool -- me da True se houver interseccao entre uma reta e um poligono
intersecRetaPoligonoBool reta poligono = [] /= [g | g<-pontosRetaPol, intersecRetasBool reta g == True]
                                           where pontosRetaPol = [[poligono !! x, poligono !! (x+1)] | x<-[0..length poligono - 2]] ++ [[poligono !! 0, poligono !! (length poligono - 1)]]
equacaoreta :: Reta -> Coeficientes
equacaoreta [(x1, y1), (x2, y2)] = (a, b)
                 where  a  = (y2 - y1) / (x2 - x1)
                        b  = y1 - (a * x1)

intersecPoligonosBool :: Poligono -> Poligono -> Bool 
intersecPoligonosBool poligono1 poligono2 = [] /= [g | g<-pontosRetaPol1, f<-pontosRetaPol2, intersecRetasBool f g == True]
                                           where pontosRetaPol1 = [[poligono1 !! x, poligono1 !! (x+1)] | x<-[0..length poligono1 - 2]] ++ [[poligono1 !! 0, poligono1 !! (length poligono1 - 1)]]
                                                 pontosRetaPol2 = [[poligono2 !! x, poligono2 !! (x+1)] | x<-[0..length poligono2 - 2]] ++ [[poligono2 !! 0, poligono2 !! (length poligono2 - 1)]]

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
centroidePoligono poligono = (abs a, abs b)
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


                                         
          
