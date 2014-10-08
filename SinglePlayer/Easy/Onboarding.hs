import System.IO
import Control.Monad
import Control.Applicative

data Enemy = Enemy { name :: String, dist :: Int } deriving (Show, Read)

readEnemy :: String -> Enemy
readEnemy str = Enemy name dist
    where input = words str
          name = input!!0
          dist = read (input!!1) :: Int

nearestEnemy :: [Enemy] -> Enemy
nearestEnemy xs = head [x | x <- xs, dist x == minimum (map dist xs) ]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    loop

loop :: IO ()
loop = do
    count <- fmap (read :: String -> Int) getLine
    enemies <- replicateM count getLine -- enemies == ["AAA 20", "BBB 30", "CCC 10"]
    putStrLn . name $ nearestEnemy $ map readEnemy $ enemies

    loop
