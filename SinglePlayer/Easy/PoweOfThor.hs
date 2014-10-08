import System.IO
import Control.Monad
import Control.Applicative

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let input = map (read :: String -> Int) $ words input_line
    loop input

loop :: [Int] -> IO ()
loop pos = do
    e <- (read :: String -> Int) <$> getLine -- The level of Thor's remaining energy, representing the number of moves he can still make.
    hPutStrLn stderr $ "e = " ++ show e ++ "\n"
    
    -- A single line providing the move to be made: N NE E SE S SW W or NW
    putStrLn "SE"
    
    loop pos

calcPos :: [Int] -> ([Int], String)
calcPos pos = (newPos, dir)
    where lx = pos!!0 -- the X position of the light of power
          ly = pos!!1 -- the Y position of the light of power
          tx = pos!!2 -- Thor's starting X position
          ty = pos!!3 -- Thor's starting Y position
          newPos = pos
          dir = calcDir pos
          calcDir = dy ++ dx
          dy = 