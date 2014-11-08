import System.IO
import Control.Monad

type Coord = (Int, Int)

readCoord :: String -> Coord -- "1000 100" -> (1000, 100)
readCoord str = (input!!0, input!!1)
    where input = map (read :: String -> Int) $ words str

landNthLength :: [Coord] -> Int -> (Coord, Int) -- [Coords], n to (Coords, flatLandLength)
landNthLength arr n = getLen $ takeWhile (\x -> snd x == snd (arr' !! 0) ) arr'
    where arr' = drop n arr
          getLen res = (head res, fst (last res) - fst (head res))

mapLandLength :: [Coord] -> [(Coord, Int)] -- [Coords] to [(Coords, flatLandLength)]
mapLandLength coords = map (landNthLength coords) [0..(length coords - 1)]

mostOpenPoint :: [(Coord, Int)] -> (Coord, Int)
mostOpenPoint = foldr (\x acc -> if snd x < snd acc then acc else x) ((0, 0), 0)

landPoint :: (Coord, Int) -> Coord
landPoint (pos, w) = (fst pos + (w `div` 2), snd pos)

calcLandPoint :: [Coord] -> Coord
calcLandPoint arr = landPoint $ mostOpenPoint $ mapLandLength arr

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    n <- fmap (read :: String -> Int) getLine -- the number of points used to draw the surface of Mars.

    coords <- replicateM n getLine -- coords == ["0 100", "1000 500", "1500 100"]
    let landingPoint = map readCoord coords -- select a x coordinate to land, which is center of largest flat land
    hPutStrLn stderr $ show landingPoint
    hPutStrLn stderr $ show $ calcLandPoint landingPoint

    loop $ calcLandPoint landingPoint

loop :: Coord -> IO ()
loop land = do
    input_line <- getLine
    let input = words input_line
    let x = read (input!!0) :: Int
    let y = read (input!!1) :: Int
    let hs = read (input!!2) :: Int -- the horizontal speed (in m/s), can be negative.
    let vs = read (input!!3) :: Int -- the vertical speed (in m/s), can be negative.
    let f = read (input!!4) :: Int -- the quantity of remaining fuel in liters.
    let r = read (input!!5) :: Int -- the rotation angle in degrees (-90 to 90).
    let p = read (input!!6) :: Int -- the thrust power (0 to 4).

    -- hPutStrLn stderr "Debug messages..."
    let outr = setAngle land x y
    let outp = setPower land y

    -- R P. R is the desired rotation angle. P is the desired thrust power.
    putStrLn $ show outr ++ " " ++ show outp

    loop land

setAngle :: Coord -> Int -> Int -> Int
setAngle pos x y
    | y < (snd pos) + 1000 = 0
    | x < (fst pos - 20)  = negate 10
    | (x + 20) > fst pos  = 10
    | otherwise           = 0

setPower :: Coord -> Int -> Int
setPower pos y
    | y < (snd pos) + 1000 = 4
    | otherwise = 3
