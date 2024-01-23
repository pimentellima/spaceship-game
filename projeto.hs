{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

main = do
  activityOf mundoinicial update visualization

data SpaceWorld = SpaceWorld {nave1, nave2 :: Nave}

data Nave = Nave
  { posicao :: (Double, Double),
    velocidade :: (Double, Double),
    angulo :: Double,
    orientacao :: Double,
    aceleracao :: Bool,
    projetilTempo :: Double,
    destruicao :: Int,
    projeteis :: [Projetil]
  }

data Projetil = Projetil
  { posicaoProj :: (Double, Double),
    tempoProj :: Double,
    anguloProj :: Double,
    velocidadeProj :: (Double, Double)
  }

mundoinicial = SpaceWorld {nave1 = nave1Inicial, nave2 = nave2Inicial}

nave1Inicial =
  Nave
    { posicao = (0, 8),
      velocidade = (0, 0),
      angulo = -pi / 2,
      orientacao = 0,
      aceleracao = False,
      projetilTempo = 0,
      destruicao = 0,
      projeteis = []
    }

nave2Inicial =
  Nave
    { posicao = (0, -8),
      velocidade = (0, 0),
      angulo = pi / 2,
      orientacao = 0,
      aceleracao = False,
      projetilTempo = 0,
      destruicao = 0,
      projeteis = []
    }

--------------------------------- Função de visualização

visualization :: SpaceWorld -> Picture
visualization (SpaceWorld n1 n2) =
  translated posXNave1 posYNave1 (rotated (angulo n1) $ colored white fignave) &
    translated posXNave2 posYNave2 (rotated (angulo n2) $ colored grey fignave) &
      pictures (map (\projetil -> translated (fst $ posicaoProj projetil) (snd $ posicaoProj projetil) $ rotated (anguloProj projetil) $ colored red figprojetil) $ projeteis $ n1) &
        pictures (map (\projetil -> translated (fst $ posicaoProj projetil) (snd $ posicaoProj projetil) $ rotated (anguloProj projetil) $ colored yellow figprojetil) $ projeteis $ n2) &
          colored black $
            solidPolygon [(-15, 15), (15, 15), (15, -15), (-15, -15)]
  where
    fignave = solidPolygon (naveCentroide [(0, -0.5), (0, 0.5), (1, 0)])
    figprojetil = solidPolygon [(0, 0.1), (0.5, 0.1), (0.5, -0.1), (0, -0.1)]

    (posXNave1, posYNave1) = posicao $ n1
    (posXNave2, posYNave2) = posicao $ n2

--------------------------------- Função de atualização

update :: Event -> SpaceWorld -> SpaceWorld
update (KeyPress "Enter") (SpaceWorld n1 n2) = geraProjetil 1 (SpaceWorld n1 n2)
update (KeyPress "Up") (SpaceWorld n1 n2) = (SpaceWorld n1 n2) {nave1 = n1 {aceleracao = True}}
update (KeyRelease "Up") (SpaceWorld n1 n2) = (SpaceWorld n1 n2) {nave1 = n1 {aceleracao = False}}
update (KeyPress "Left") (SpaceWorld n1 n2) = (SpaceWorld n1 n2) {nave1 = n1 {orientacao = 1}}
update (KeyRelease "Left") (SpaceWorld n1 n2) = (SpaceWorld n1 n2) {nave1 = n1 {orientacao = 0}}
update (KeyPress "Right") (SpaceWorld n1 n2) = (SpaceWorld n1 n2) {nave1 = n1 {orientacao = (-1)}}
update (KeyRelease "Right") (SpaceWorld n1 n2) = (SpaceWorld n1 n2) {nave1 = n1 {orientacao = 0}}
update (KeyPress " ") (SpaceWorld n1 n2) = geraProjetil 2 (SpaceWorld n1 n2)
update (KeyPress "W") (SpaceWorld n1 n2) = (SpaceWorld n1 n2) {nave2 = n2 {aceleracao = True}}
update (KeyRelease "W") (SpaceWorld n1 n2) = (SpaceWorld n1 n2) {nave2 = n2 {aceleracao = False}}
update (KeyPress "A") (SpaceWorld n1 n2) = (SpaceWorld n1 n2) {nave2 = n2 {orientacao = 1}}
update (KeyRelease "A") (SpaceWorld n1 n2) = (SpaceWorld n1 n2) {nave2 = n2 {orientacao = 0}}
update (KeyPress "D") (SpaceWorld n1 n2) = (SpaceWorld n1 n2) {nave2 = n2 {orientacao = (-1)}}
update (KeyRelease "D") (SpaceWorld n1 n2) = (SpaceWorld n1 n2) {nave2 = n2 {orientacao = 0}}
update (TimePassing t) (SpaceWorld n1 n2) = projetilNaves t . movimentaNaves t . destruicaoNaves $ fimJogo (SpaceWorld n1 n2)
update _ w = w

--------------------------------- Movimentos

movimentaNaves t (SpaceWorld n1 n2) =
  (SpaceWorld n1 n2)
    { nave1 = novaPosicao t . novaVelocidade t . novoAngulo t $ n1,
      nave2 = novaPosicao t . novaVelocidade t . novoAngulo t $ n2
    }

novaPosicao t nave = nave {posicao = (novoX, novoY)}
  where
    novoX = x + vx * t + 1 / 2 * ax * t ^ 2
    novoY = y + vy * t + 1 / 2 * ay * t ^ 2

    (vx, vy) = velocidade nave
    (x, y) = posicao nave
    (ax, ay) = aceleracaoNave nave

novaVelocidade t nave = nave {velocidade = (novaVelX, novaVelY)}
  where
    (novaVelX, novaVelY) = (vx + ax * t, vy + ay * t)
    (vx, vy) = velocidade nave
    (ax, ay) = aceleracaoNave nave

aceleracaoNave nave
  | aceleracao nave && destruicao nave < 11 = (2 * cos av, 2 * sin av)
  | otherwise = (0, 0)
  where
    af = orientacao nave
    av = angulo nave

novoAngulo t nave = nave {angulo = velAngular nave}
  where
    af = orientacao nave
    av = angulo nave
    velAngular nave
      | destruicao nave < 11 = av + af * pi * t
      | otherwise = av

projetilNaves t (SpaceWorld n1 n2) =
  (SpaceWorld n1 n2)
    { nave1 =
        n1
          { projeteis = viajaProjetil t (projeteis n1),
            projetilTempo = reduztempoProjetil t $ n1
          },
      nave2 =
        n2
          { projeteis = viajaProjetil t (projeteis n2),
            projetilTempo = reduztempoProjetil t $ n2
          }
    }

geraProjetil nave (SpaceWorld n1 n2)
  | nave == 1 = (SpaceWorld n1 n2) {nave1 = projetilNave n1}
  | nave == 2 = (SpaceWorld n1 n2) {nave2 = projetilNave n2}

projetilNave nave
  | projetilTempo nave < 0.9 && destruicao nave < 11 = nave {projeteis = projetilInicial nave : projeteis nave, projetilTempo = 1}
  | otherwise = nave

projetilInicial nave =
  Projetil
    { posicaoProj = posInicial,
      tempoProj = 5,
      anguloProj = anguloInicial,
      velocidadeProj = velInicial
    }
  where
    posInicial = (x + cos anguloInicial, y + sin anguloInicial)
    (x, y) = posicao nave
    anguloInicial = angulo nave
    (vx, vy) = velocidade nave
    velInicial = (vx, vy)

viajaProjetil t [] = []
viajaProjetil t (x : xs)
  | tempoProj x > 0 = x {tempoProj = (tempoProj x) - t, posicaoProj = (px, py)} : viajaProjetil t xs
  | tempoProj x <= 0 = viajaProjetil t xs
  where
    (velXPJ, velYPJ) = velocidadeProj x
    (apx, apy) = posicaoProj x
    ap = anguloProj x
    px = apx + abs velXPJ * t * cos ap + 2 * t * cos ap
    py = apy + abs velYPJ * t * sin ap + 2 * t * sin ap

reduztempoProjetil t nave
  | projetilTempo nave <= 0 = 0
  | projetilTempo nave > 0 = projetilTempo nave - t * 0.35

destruicaoNaves (SpaceWorld n1 n2) =
  (SpaceWorld n1 n2)
    { nave1 = n1 {destruicao = atualizadestruicao n1 1 (destruicao n1) listaproj, projeteis = removeProjetil (SpaceWorld n1 n2) pN1},
      nave2 = n2 {destruicao = atualizadestruicao n2 2 (destruicao n2) listaproj, projeteis = removeProjetil (SpaceWorld n1 n2) pN2}
    }
  where
    pN1 = projeteis n1
    pN2 = projeteis n2
    listaproj = pN1 ++ pN2

atualizadestruicao nave n dest [] = dest
atualizadestruicao nave n dest (a : as) = dest + colisao nave n a + atualizadestruicao nave n 0 as
  where
    colisao nave n a
      | checaColisao nave n a = 1
      | otherwise = 0

checaColisao nave n z
  | n == 1 && intersecPoligonos figNave1 (figprojetil z) = True
  | n == 2 && intersecPoligonos figNave2 (figprojetil z) = True
  | otherwise = False
  where
    figprojetil z = [(0 + xPJ, 0.1 + yPJ), (0.5 + xPJ, 0.1 + yPJ), (0.5 + xPJ, -0.1 + yPJ), (0 + xPJ, -0.1 + yPJ)]
    (xPJ, yPJ) = posicaoProj z

    figNave1 = [(0 + xN1, -0.5 + yN1), (0 + xN1, 0.5 + yN1), (1 + xN1, 0 + yN1)]
    figNave2 = [(0 + xN2, -0.5 + yN2), (0 + xN2, 0.5 + yN2), (1 + xN2, 0 + yN2)]

    (xN1, yN1) = posicao nave
    (xN2, yN2) = posicao nave

removeProjetil (SpaceWorld n1 n2) lista = filter (\projetil -> not (checaColisao n1 1 projetil) && not (checaColisao n2 2 projetil)) lista

fimJogo (SpaceWorld n1 n2)
  | destruicao n1 >= 11 = (SpaceWorld n1 n2) {nave1 = n1 {posicao = (20, 20)}}
  | destruicao n2 >= 11 = (SpaceWorld n1 n2) {nave2 = n2 {posicao = (20, 20)}}
  | otherwise = (SpaceWorld n1 n2)

--------------------------------- Funções geometricas

type Reta = (Ponto, Ponto)

type Ponto = (Double, Double)

type Poligono = [Ponto]

type Coeficientes = (Double, Double)

naveCentroide :: Poligono -> Poligono
naveCentroide [(x1, y1), (x2, y2), (x3, y3)] = [(x1 - fst centroide, y1 - snd centroide), (x2 - fst centroide, y2 - snd centroide), (x3 - fst centroide, y3 - snd centroide)]
  where
    centroide = centroidePoligono [(x1, y1), (x2, y2), (x3, y3)]

checaIntersec :: Reta -> Reta -> Bool
checaIntersec (p1, q1) (p2, q2)
  | orientacaoSeg p1 q1 p2 /= orientacaoSeg p1 q1 q2
      && orientacaoSeg p2 q2 p1 /= orientacaoSeg p2 q2 q1 =
      True
  | orientacaoSeg p1 q1 p2 == 0
      && orientacaoSeg p1 q1 q2 == 0
      && orientacaoSeg p2 q2 q1 == 0
      && orientacaoSeg p2 q2 q1 == 0
      && interceptaProj (p1, q1) (p2, q2) =
      True
  | otherwise = False

interceptaProj (p1, q1) (p2, q2) =
  max (fst p1) (fst q1) >= min (fst p2) (fst q2)
    && min (fst p1) (fst q1) <= max (fst p2) (fst q2)
    && max (snd p1) (snd q1) >= min (snd p2) (snd q2)
    && min (snd p1) (fst q1) <= max (snd p2) (snd q2)

orientacaoSeg p q r
  | prodVet > 0 = 1
  | prodVet < 0 = 2
  | otherwise = 0
  where
    prodVet = ((snd q - snd p) * (fst r - fst q)) - ((fst q - fst p) * (snd r - snd q))

intersecPoligonos :: Poligono -> Poligono -> Bool
intersecPoligonos polig1 polig2 = intersecRetasPol1Reta polig1 $ segmentosDoPol polig2

intersecRetasPol1Reta polig1 [] = False
intersecRetasPol1Reta polig1 (x : xs)
  | intersecRetaPolig polig1 x = True
  | otherwise = intersecRetasPol1Reta polig1 xs

intersecRetaPolig polig reta = intersecRetasPolReta (segmentosDoPol polig) reta

intersecRetasPolReta [] reta = False
intersecRetasPolReta (x : xs) reta
  | checaIntersec x reta = True
  | otherwise = intersecRetasPolReta xs reta

segmentosDoPol :: Poligono -> [Reta]
segmentosDoPol pol = zip pol (tail pol ++ [head pol])

areaPoligonoR :: Poligono -> Double
areaPoligonoR ps =
  1
    / 2
    * ( sum
          [ fst p1 * snd p2 - fst p2 * snd p1
            | (p1, p2) <- zip ps (tail ps ++ [head ps])
          ]
      )

centroidePoligono :: Poligono -> Point
centroidePoligono pol = (cx, cy)
  where
    cx =
      1
        / (6 * area)
        * sum
          [ (x1 + x2) * (x1 * y2 - x2 * y1)
            | ((x1, y1), (x2, y2)) <- segmentos
          ]
    cy =
      1
        / (6 * area)
        * sum
          [ (y1 + y2) * (x1 * y2 - x2 * y1)
            | ((x1, y1), (x2, y2)) <- segmentos
          ]
    segmentos = segmentosDoPol pol
    area = areaPoligonoR pol
