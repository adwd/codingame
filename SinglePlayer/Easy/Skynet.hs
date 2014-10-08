import System.IO
import Control.Monad
import Control.Applicative

data BikeInfo = BikeInfo { speed :: Int, position :: Int } deriving (Show, Read)
data InitInput = InitInput { r :: Int, g :: Int, l :: Int} deriving (Show, Read)

readInit :: [Int] -> InitInput
readInit (r:g:l:[]) = InitInput r g l

showInit :: InitInput -> String
showInit i = "before gap = " ++ (show $ r i) ++ ", gap = " ++ (show $ g i) ++ ", land = " ++ (show $ l i)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    init_input <- fmap (read :: String -> Int) <$> replicateM 3 getLine
    loop $ readInit init_input

loop :: InitInput -> IO ()
loop init_input = do
    input_line <- getLine
    let s = read input_line :: Int -- the motorbike's speed.
    input_line <- getLine
    let x = read input_line :: Int -- the position on the road of the motorbike.
    hPutStrLn stderr $ "speed = " ++ show s ++ ", position = " ++ show x ++ "\n"
    hPutStrLn stderr $ showInit init_input

    if r init_input == x + 1 then do putStrLn "JUMP"
                                     stoploop
                             else do putStrLn $ calcSpeed s $ g init_input 
                                     loop init_input

calcSpeed :: Int -> Int -> String
calcSpeed s g | s == g + 1 = "WAIT"
              | s > g + 1 = "SLOW"
              | s < g + 1 = "SPEED"

stoploop :: IO ()
stoploop = do
    putStrLn "SLOW"
    stoploop
