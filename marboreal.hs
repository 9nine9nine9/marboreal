import Test.QuickCheck
import System.Random

xRes, yRes :: Int
xRes = 240
yRes = 200


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
          g x = gs!!(round x)

-- Interpolation polynomial
fade :: Float -> Float
fade x = x^3 * (x * (x - 15) +10)


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
perlinNoise v@(Vct x y) vs = (1 - fy) * i1 + fy * i2
    where v0 = Vct (rd x) (rd y)
          v1 = Vct (ru x) (rd y)
          v2 = Vct (rd x) (ru y)
          v3 = Vct (ru x) (ru y)
          g0 = g v0
          g1 = g v1
          g2 = g v2
          g3 = g v3
          g (Vct x y) = vs!!(round x)!!(round y)  -- Only ever called on int vects
          fx = fade (x - (rd x))
          fy = fade (y - (rd y))
          i1 = (1 - fx) * (dot g0 (v `vm` v0)) + fx * (dot g1 (v `vm` v1))
          i2 = (1 - fx) * (dot g2 (v `vm` v2)) + fx * (dot g3 (v `vm` v3))

toPix :: Float -> Int
toPix f = round $ 100 * f


-- Generate image from randoms
pixs :: [[Vct]] -> [[Int]]
pixs vss = [[toPix $ perlinNoise v vss | v <- vs] | vs <- vss]
-- pixs vss = [[toPix $ x | (Vct x y) <- vs] | vs <- vss]

mkPPM :: [[Int]] -> String
mkPPM pss = "P2\n" ++ show xRes ++ " " ++ show yRes ++ "\n" ++
            show (maximum (map maximum pss)) ++ "\n" ++
            concat ["\n" ++ concat [show p ++ " " | p <- ps] | ps <- pss]

main :: IO ()
main = do
    putStr $ mkPPM $ pixs $ take yRes $ map (take xRes) $ randV (mkStdGen 1) (mkStdGen 2)
