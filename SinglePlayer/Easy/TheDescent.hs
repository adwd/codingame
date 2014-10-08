import System.IO
import Control.Monad
import Control.Applicative

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    loop

loop :: IO ()
loop = do
    input_line <- getLine
    let input = words input_line
    let sx = read (input!!0) :: Int
    let sy = read (input!!1) :: Int
    
    input_line <- fmap (read :: String -> Int) <$> replicateM 8 getLine
    hPutStrLn stderr $ show input_line
    
    -- either:  FIRE (ship is firing its phase cannons) or HOLD (ship is not firing).
    putStrLn $ if input_line!!sx == maximum input_line
                then "FIRE"
                else "HOLD"
    
    loop
