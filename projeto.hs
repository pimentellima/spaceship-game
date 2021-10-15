{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

main = do 
          activityOf ((90,90),0,(0,0)) update visualization
         
          

type World = (Angulo, Velocidade, Posicao)
type Posicao = (Double, Double)
type Angulo = (Double, Double)
type Velocidade = Double
type Reta = [Ponto]
type Ponto = (Double, Double)
type Coeficientes = (Double, Double)
type Poligono = [Ponto]


visualization :: World -> Picture
visualization ((a,b),v, (x,y)) 
         | x >= 7 && x < 7.5 && y <= 5 && y >= -5 = pictures [styledLettering Plain SansSerif "Nave destruida. Aperte Enter para reiniciar.", colored brown (thickPolyline 0.5 [(-7,-5),(-7,5)]), 
 colored yellow (thickPolyline 0.5 [(7,-5),(7,5)])]
         | otherwise = pictures [ (translated x y (rotated (a * pi/180) nave)), colored brown (thickPolyline 0.5 [(-7,-5),(-7,5)]), 
 colored yellow (thickPolyline 0.5 [(7,-5),(7,5)])]
   where nave = solidPolygon (naveCentroide [(0, -0.5), (0,0.5), (1, 0)])


update :: Event -> World -> World
update (KeyPress "Enter") ((a,b),v, (x,y)) = ((90,90), 0, (0,0))
update _ (a,v, (x,y)) 
         | x >= 7 && x < 7.3 && y <= 5 && y >= -5 = (a,v, (x,y))   
         | y >= 10 = (a,v, (x, -y + 0.3))
         | y <= -10 = (a,v, (x, -y - 0.3))
         | x >= 10 = (a, v, (-x + 0.3, y))
         | x <= -10 = (a, v, (-x - 0.3, y))
update (KeyPress "Up") ((a,b),v, (x,y)) 
         | v >= 5 = ((a,a), 5, (x, y))
         | otherwise = ((a,a), v + 1, (x, y))  
update (KeyPress "Left") ((a,b),v, (x,y)) = ((a + 10,b), v, (x,y))
update (KeyPress "Right") ((a,b),v, (x,y)) = ((a - 10,b), v , (x,y))         
update (TimePassing t) ((a,b),v, (x,y)) 
                                  | novoX <= -7 && novoX > -7.25 && novoY >= -5 && novoY <= 5 = ((2 * (-a), 2 * (-b)), v * 0.8, (x,y))
                                  | v <= 0 = ((a,b),0, (x,y)) 
                                  | otherwise = ((a,b),v - 0.5 * t, (novoX, novoY))
                                       where novoX = x + cos (b * pi/180) * v * t 
                                             novoY = y + sin (b * pi/180) * v * t  
update _ (a,v, (x,y)) = (a,v, (x,y))                                                      

---- Funções geométricas
 
intersecRetasBool :: Reta -> Reta -> Bool -- Verifica se dois segmentos de reta se intersectam 
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
                                                             
equacaoreta :: Reta -> Coeficientes -- Função auxiliar que calcula os coeficientes da reta
equacaoreta [(x1, y1), (x2, y2)] = (a, b)
                 where  a  = (y2 - y1) / (x2 - x1)
                        b  = y1 - (a * x1)
                                                                                                    
                                                             
intersecRetaPoligonoBool :: Reta -> Poligono -> Bool -- Verifica se um polígono e um segmento de reta se intersectam 
intersecRetaPoligonoBool reta poligono = [] /= [g | g<-pontosRetaPol, intersecRetasBool reta g == True]
                                           where pontosRetaPol = [[poligono !! x, poligono !! (x+1)] | x<-[0..length poligono - 2]] ++ [[poligono !! 0, poligono !! (length poligono - 1)]]

intersecPoligonosBool :: Poligono -> Poligono -> Bool  -- Verifica se dois polígonos se intersectam 
intersecPoligonosBool poligono1 poligono2 = [] /= [g | g<-pontosRetaPol1, f<-pontosRetaPol2, intersecRetasBool f g == True]
                                           where pontosRetaPol1 = [[poligono1 !! x, poligono1 !! (x+1)] | x<-[0..length poligono1 - 2]] ++ [[poligono1 !! 0, poligono1 !! (length poligono1 - 1)]]
                                                 pontosRetaPol2 = [[poligono2 !! x, poligono2 !! (x+1)] | x<-[0..length poligono2 - 2]] ++ [[poligono2 !! 0, poligono2 !! (length poligono2 - 1)]]

areaPoligono :: Poligono -> Double -- Retorna a área de um polígono
areaPoligono poligono =  abs ( ( (sum [x1 * y2 - x2 *  y1 | x<-[0..length poligono - 2],  let x1 = fst (poligono !! x)
                                                                                              x2 = fst (poligono !! ((x+1)))
                                                                                              y1 = snd (poligono !! x)
                                                                                              y2 = snd (poligono !! (x+1)) ] ) 
                        + xn * y1 - x1 * yn ) / 2) 
                                                                                          where xn = fst (poligono !! (length poligono - 1))
                                                                                                y1 = snd (poligono !! 0)
                                                                                                x1 = fst (poligono !! 0)
                                                                                                yn = snd (poligono !! (length poligono - 1))
                        

centroidePoligono :: Poligono -> Ponto -- Retorna as coordenadas do centróide de um polígono
centroidePoligono poligono = (-a,  -b)
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
                                                                                                              
reflexoAngulo :: Vector -> Vector -- Retorna o reflexo do vetor
reflexoAngulo vetor = vectorSum (rotatedVector (2*a) vetor) vetor
                         where a = vectorDirection vetor
                                                                                                              
pontoIntersecRetas :: Reta -> Reta -> Ponto -- Retorna o ponto de intersecção de dois segmentos de reta
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

naveCentroide :: Poligono -> Poligono -- Função auxiliar que ajusta o centroide da nave para as coordenadas (0,0)
naveCentroide [(x1, y1), (x2,y2), (x3, y3)] = [(x1 - fst centroide, y1 - snd centroide), (x2 - fst centroide, y2 - snd centroide), (x3 - fst centroide, y3 - snd centroide)]
  where centroide = centroidePoligono [(x1, y1), (x2,y2), (x3, y3)]


                                         
          
