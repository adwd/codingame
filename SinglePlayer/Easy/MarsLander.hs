import System.IO
import Control.Monad

type Coord = (Int, Int)

readCoord :: String -> Coord -- "1000 100" -> (1000, 100)
readCoord str = (input!!0, input!!1)
    where input = map (read :: String -> Int) $ words str

--land :: [Coord] -> Int -- [(0,100),(1000,500),(1500,100),(3000,100)] -> 2250

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    n <- fmap (read :: String -> Int) getLine -- the number of points used to draw the surface of Mars.

    coords <- replicateM n getLine -- coords == ["0 100", "1000 500", "1500 100"]
    let landingPoint = map readCoord coords -- select a x coordinate to land, which is center of largest flat land
    hPutStrLn stderr $ show landingPoint

    loop

loop :: IO ()
loop = do
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
    
    -- R P. R is the desired rotation angle. P is the desired thrust power.
    putStrLn "-20 3"
    
    loop
