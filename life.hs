module Main where
import Data.List (group, sort, concatMap)
import Data.Set

lifeStep :: Set (Int, Int) -> Set (Int, Int)
lifeStep cells = fromList [head g | g <- grouped cells, viable g]
  where grouped = group . sort . concatMap neighbors . toList
        neighbors (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0)]
        viable [_,_,_] = True
        viable [c,_] = c `member` cells
        viable _ = False

showWorld :: Set (Int, Int) -> IO ()
showWorld cells = mapM_ putStrLn $ worldToGrid cells
  where worldToGrid cells = [[cellChar (x, y) | x <- [least..greatest]] | y <- [least..greatest]]
        cellChar cell = if cell `member` cells then '#' else '.'
        (least, greatest) = worldBounds cells

worldBounds :: Set (Int, Int) -> (Int, Int)
worldBounds cells = (least, greatest)
  where lst = toList cells
        least = min x y
        greatest = max x' y'
        (x, y) = head lst
        (x', y') = last lst

runLife :: Int -> Set (Int, Int) -> IO ()
runLife steps cells = rec (steps - 1) cells
  where rec 0 cells = showWorld cells
        rec s cells = do showWorld cells
                         putStrLn ""
                         rec (s - 1) $ lifeStep cells

glider = fromList [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = fromList [(1, 0), (1, 1), (1, 2)]

main :: IO ()
main = do
  putStrLn "Glider >> 10"
  putStrLn "------------"
  runLife 10 glider
  putStrLn ""
  putStrLn "Blinker >> 3"
  putStrLn "------------"
  runLife 3 blinker