import System.Random

xRes, yRes :: Int
xRes = 50
yRes = 50

scale :: Float
scale = 0.4


rd, ru :: Float -> Float
rd x = fromIntegral $ floor x
ru x = fromIntegral $ ceiling x

-- Random int from [1, -1]
randB :: StdGen -> [Bool]
randB gen = randoms gen

signs :: [Bool] -> [Float]
signs = map (\x -> fromIntegral $ 2*(fromEnum x)-1)

gradNoise :: Float -> [Float] -> Float
gradNoise p gs = (1 - fade (p-p0)) * (g p0) * (p-p0) + fade (p-p0) * (g p1) * (p-p1)
    where p0 = rd p
          p1 = ru p
          g x = gs!!(floor x)


clamp l h x = if x < h then max l x else min x h
-- Perlin's smootherstep polynomial
fade :: Float -> Float
fade x = clamp 0 1 $ x^3 * (x * (x*6 - 15) + 10)


data Vct = Vct Float Float
    deriving (Eq, Ord, Show)

vp :: Vct -> Vct -> Vct
(Vct a b) `vp` (Vct c d) = Vct (a+c) (b+d)
vm :: Vct -> Vct -> Vct
(Vct a b) `vm` (Vct c d) = Vct (a-c) (b-d)

pyth :: Vct -> Float
pyth (Vct a b) = sqrt (a^2 + b^2)

dot :: Vct -> Vct -> Float
dot (Vct x y) (Vct a b) = x*a + y*b

scaleVct :: Vct -> Vct
scaleVct v@(Vct x y) = Vct (x/l) (y/l)
    where l = pyth v

randV :: StdGen -> StdGen -> [[Vct]]
randV gen1 gen2 = [[scaleVct (Vct x y) | y <- randoms gen2] | x <- randoms gen1]

perlinNoise :: Vct -> [[Vct]] -> Float
perlinNoise v@(Vct x y) vs = (i1-i0)*fy + i0
    where v0 = Vct (rd x) (rd y)
          v1 = Vct (ru x) (rd y)
          v2 = Vct (rd x) (ru y)
          v3 = Vct (ru x) (ru y)
          g (Vct x y) = vs!!(round x)!!(round y)  -- Only ever called on int vects
          d0 = dot (g v0) (v `vm` v0) -- Gradient vectors dotted with offset vectors
          d1 = dot (g v1) (v `vm` v1)
          d2 = dot (g v2) (v `vm` v2)
          d3 = dot (g v3) (v `vm` v3)
          fx = fade (x - (rd x))
          fy = fade (y - (rd y))
--           i1 = (1 - fx) * (dot g0 o0) + fx * (dot g1 o1)
--           i2 = (1 - fx) * (dot g2 o2) + fx * (dot g3 o3)
          i0 = (d1-d0)*fx + d0
          i1 = (d3-d2)*fx + d2

toPix :: [[Float]] -> [[Int]]
toPix fss = map (map (max 0)) $ map (map floor) scaledfss
    where minf = minimum (map minimum fss)
          min0 = map (map (\x -> x-minf)) fss
          maxf = maximum (map maximum min0)
          scaledfss = map (map (*(255.0/maxf))) min0


-- Generate image from randoms
pixs :: [[Vct]] -> [[Float]]
pixs vss = [[perlinNoise (Vct x y) vss
                    | y <- [0.0,scale..scale*(fromIntegral yRes-1)]]
                    | x <- [0.0,scale..scale*(fromIntegral xRes-1)]]

mkPPM :: [[Int]] -> String
mkPPM pss = "P2\n" ++ show xRes ++ " " ++ show yRes ++ "\n" ++
            show (maximum (map maximum pss)) ++
            concat ["\n" ++ concat [show p ++ " " | p <- ps] | ps <- pss]

main :: IO ()
main = do
    putStr $ mkPPM $ toPix $ pixs $ take (yRes+1) $ map (take (xRes+1)) $ randV (mkStdGen 98342) (mkStdGen 193732)
