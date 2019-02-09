Module to solve mazes.

> module MazeSolver (
>   Path,                  -- :: [Direction]
>   solveMaze              -- :: Maze -> Place -> Place -> Path
> )
> where

> import Geography
> import Maze
> import MazeGenerator
> import Data.List

> type Path = [Direction]

> solveMaze :: Maze -> Place -> Place -> Path --solves a maze
> solveMaze maze start target = solveMazeIter maze target initialPartials initialVisited
>     where initialPartials = [(start,[])]
>           initialVisited  = updateVisited zeroVisited initialPartials
>           zeroVisited     = replicate m (replicate n False)
>           (m,n)           = sizeOf maze

> solveMazeIter :: Maze -> Place -> [(Place,Path)] -> [[Bool]] -> Path --performs BFS
> solveMazeIter _    _      []       _       = error "There is no path."
> solveMazeIter maze target partials visited = if pathFound then pathToTarget else solveMazeIter maze target newPartials newVisited
>     where (pathFound,pathToTarget) = extractPathToTarget target partials
>           newPartialsN             = concat (map (getNeighbour maze visited N) partials)
>           newVisitedN              = updateVisited visited newPartialsN
>           newPartialsW             = concat (map (getNeighbour maze newVisitedN W) partials)
>           newVisitedNW             = updateVisited newVisitedN newPartialsW
>           newPartialsS             = concat (map (getNeighbour maze newVisitedNW S) partials)
>           newVisitedNWS            = updateVisited newVisitedNW newPartialsS
>           newPartialsE             = concat (map (getNeighbour maze newVisitedNWS E) partials)
>           newVisited               = updateVisited newVisitedNWS newPartialsE
>           newPartials              = newPartialsN ++ newPartialsW ++ newPartialsS ++ newPartialsE

They are split up like that, so we don't get duplicated paths of equal length to some place at the same step, this is a huge problem on relatively empty mazes otherwise.

> extractPathToTarget :: Place -> [(Place,Path)] -> (Bool,Path) --returns (True, desired path) if the path to the target is found otherwise (False, [])
> extractPathToTarget target []                = (False,[])
> extractPathToTarget target ((place,path):xs) = if place == target then (True,path) else extractPathToTarget target xs

> getNeighbour :: Maze -> [[Bool]] -> Direction -> (Place,Path) -> [(Place,Path)] --returns the partial in some direction
> getNeighbour maze visited direction (place,path) = if isAccessible maze place direction && isNotVisited visited neighbour then [(neighbour,path ++ [direction])] else []
>     where neighbour = move direction place

> isAccessible :: Maze -> Place -> Direction -> Bool --checks whether a neighbour is accessible
> isAccessible maze place direction = not (hasWall maze place direction)

> isNotVisited :: [[Bool]] -> Place -> Bool --checks whether a place is not visited
> isNotVisited visited (i,j) = not (visited !! i !! j)

> updateVisited :: [[Bool]] -> [(Place,Path)] -> [[Bool]] --updates the visited list
> updateVisited visited partials = updateVisitedIter visited (Data.List.sort (map f partials)) 0
>     where f (x,y) = x

> updateVisitedIter :: [[Bool]] -> [Place] -> Int -> [[Bool]] --goes trough all columns of visited to update them
> updateVisitedIter []       _  _    = []
> updateVisitedIter xss      [] _    = xss
> updateVisitedIter (xs:xss) ps col  = updateVisitedColumnIter xs ps1 0 : updateVisitedIter xss ps2 (col+1)
>     where ps1     = takeWhile f ps
>           ps2     = dropWhile f ps
>           f (i,j) = i == col

> updateVisitedColumnIter :: [Bool] -> [Place] -> Int -> [Bool] --goes trough all elements of a list to update them
> updateVisitedColumnIter []     _      _    = []
> updateVisitedColumnIter xs     []     _    = xs
> updateVisitedColumnIter (x:xs) (p:ps) row  = (x || f p) : updateVisitedColumnIter xs ps2 (row+1)
>     where ps2     = dropWhile f (p:ps)
>           f (i,j) = j == row

