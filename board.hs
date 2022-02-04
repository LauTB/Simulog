type Coord = (Int, Int)

boardRowSize::Int
boardRowSize = 4
boardColSize::Int
boardColSize = 3
childrenCount::Int
childrenCount = 2
corralCount::Int
corralCount = 2
obstaclesCount::Int
obstaclesCount = 2
timeUnits::Int
timeUnits = 8 --for the random enviorment change
dirtChance :: Int
dirtChance = 50 --chances of a child creates dirt when possible (in percent)


data Env = Env  {obstacles :: [Coord]
                ,children  :: [Coord]
                ,corral    :: [Coord]
                ,dirt      :: [Coord]
                ,robot     :: Coord
                ,time      :: Int
                ,turn      :: Int
                ,board     :: [[Char]]
                ,robotHacChild :: Bool
                ,robotPath :: [Coord]}



makeBoard = [['0','s','0'],
            ['0','s','0'],
            ['n','0','0'],
            ['c','0','r']]

start = do
        printBoard (board initialEnv)
        print "Simulation ended"
        let env = startSim initialEnv
        putStrLn $ "This is the final board after: " ++ show (time env) ++ " time units"
        printBoard (board env)

                -- currentEnv turn(0: move moment 1: ambient changes)
startSim env = runSim initialEnv 0
runSim env 0 = let 
                  newRobotPosEnv = moveRobot env (robot env) (robotPath env)
                  newChildrenPosEnv = moveChildren env
                in
                  runSim newChildrenPosEnv 1

runSim env 1 = initialEnv




initialEnv = Env{board         = makeBoard
                ,children      =  placeChildren childrenCount (board initialEnv) []
                ,corral        = []
                ,dirt          = []
                ,robot         = (0,0)
                ,time          = 0
                ,turn          = 0
                ,obstacles     = placeObstacles obstaclesCount board []
                ,robotHacChild = False
                ,robotPath = []
                } 
placeChildren::Int ->[[a]]->[Coord] ->[Coord]
placeChildren count board result = []
placeObstacles count board result = []
placeCorral count board result = []


printBoard [] = putStrLn ""
printBoard (x: xs) = do
    print x 
    printBoard xs

updateBoardPos :: [[a]] -> a -> Coord -> [[a]]
updateBoardPos m x (r,c) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m

-- move an object to a new position and choose what to leave behind
move :: [[a]] -> a -> Coord -> a -> Coord -> [[a]]
move m obj (nx,ny) beh (ox, oy) = let t = updateBoardPos m obj (nx,ny) in updateBoardPos t beh (ox,oy)

getValue :: [[a]] -> Coord -> a
getValue m (x,y) = m !! x!!y

getElement :: Env -> Coord -> Char
getElement e c = getValue (board e) c

isDirt :: Env -> Coord -> Bool
isDirt e c = (getElement e c) == 's'

getSurroundings :: Coord -> [Coord]
-- Up Down Left Right UpLeft UpRight DownLeft DownRight
getSurroundings (x,y) = filter valid [(x-1, y), (x+1, y),(x,y-1), (x,y+1), (x-1, y-1), (x-1,y+1), (x+1,y-1),(x+1,y+1)]
                          where valid (x,y) = (x>=0 && x < boardRowSize ) && (y>=0 && y < boardColSize)

-- enviorment robotPos dirtPos reachablePos visited closestDirtPos
--findClosestDirt :: Env -> Coord -> [Coord] -> [Coord] -> [Coord] -> Coord
findClosestDirt e (rx,ry) dirt [] visited = (-1,-1)
findClosestDirt e (rx,ry) dirt (x:xs) visited = if isDirt e x 
                                                then 
                                                  x 
                                                else 
                                                    findClosestDirt e (rx,ry) dirt (xs 
                                                      ++ (intersect visited (getSurroundings x)))
                                                      (visited ++ [x])
                                                  
moveRobot env (rx,ry) path = initialEnv
moveChildren env = initialEnv

--utils
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (x:xs) l | elem x l = x : intersect xs l
                   | otherwise = intersect xs l