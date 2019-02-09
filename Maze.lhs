Module to define the type of a maze

> module Maze (
>   Maze,
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf    -- :: Maze -> Size
> )
> where

> import Geography
> import BST

We will represent a maze by its size and BSTs of places where there are walls.

> data Maze = Maze Size (BST Place) (BST Place) (BST Place) (BST Place)

The list of walls will be complete in the sense that we record
both sides of the wall; for example, if the list includes 
((3,4), N), then it will also include ((3,5),S).

This function creates a 4-tuple of places corresponding to a complete list ofthe potentially incomplete list of walls it is given.

> makeCompleteWalls :: [Wall] -> ([Place],[Place],[Place],[Place])
> makeCompleteWalls []           = ([],[],[],[])
> makeCompleteWalls ((pos,d):ws) | d == N = (pos:newN, newW, move N pos:newS, newE)
>                                | d == W = (newN, pos:newW, newS, move W pos:newE)
>                                | d == S = (move S pos:newN, newW, pos:newS, newE)
>                                | d == E = (newN, move E pos:newW, newS, pos:newE)
>     where (newN,newW,newS,newE) = makeCompleteWalls ws

This function creates a maze given its size and a list of walls; 
the list of walls might not be complete in the above sense.

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) ws = Maze (x,y) (makeBST wallsN) (makeBST wallsW) (makeBST wallsS) (makeBST wallsE)
>     where (wallsN,wallsW,wallsS,wallsE) = makeCompleteWalls (ws ++ boundaries)
>           boundaries = -- the four boundaries
>            [((0,j),   W) | j <- [0..y-1]] ++ -- westerly boundary
>            [((x-1,j), E) | j <- [0..y-1]] ++ -- easterly boundary
>            [((i,0),   S) | i <- [0..x-1]] ++ -- southerly boundary
>            [((i,y-1), N) | i <- [0..x-1]]    -- northerly boundary

The following function tests whether the maze includes a wall in a particular
direction from a particular place:

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (Maze _ wallsN wallsW wallsS wallsE) pos d | d == N = pos `isInBST` wallsN
>                                                    | d == W = pos `isInBST` wallsW
>                                                    | d == S = pos `isInBST` wallsS
>                                                    | d == E = pos `isInBST` wallsE

The following function returns the size of a maze:

> sizeOf :: Maze -> Size
> sizeOf (Maze size _ _ _ _) = size


> instance Show Maze where
>     show maze = concat [horzWalls maze row ++ vertWalls maze row | row <- reverse [0..y-1]] ++ horzWalls maze (-1)
>         where (x,y) = sizeOf maze


> horzWalls :: Maze -> Int -> String --string of a horizontal wall
> horzWalls maze row = concat [horzWallPlus maze (col,row) : horzWallDashEndl maze (col,row) | col <- [0..x]]
>     where (x,y) = sizeOf maze

> horzWallPlus :: Maze -> (Int,Int) -> Char --plus or space in a position in a horizontal wall
> horzWallPlus maze (col,row) | hasWall maze (col,row) N || hasWall maze (col,row) W || hasWall maze (col-1,row+1) S || hasWall maze (col-1,row+1) E = '+'
>                             | otherwise                                                                                                            = ' '

> horzWallDashEndl :: Maze -> (Int,Int) -> String --dash or space or endl in a position in a horizontal wall
> horzWallDashEndl maze (col,row) | col == x                 = "\n" 
>                                 | hasWall maze (col,row) N = "--"
>                                 | otherwise                = "  "
>     where (x,y) = sizeOf maze

> vertWalls :: Maze -> Int -> String --string of a vertical wall
> vertWalls maze row = concat [vertWallLine maze (col,row) : vertWallEndl maze (col,row) | col <- [0..x]]
>     where (x,y) = sizeOf maze

> vertWallLine :: Maze -> (Int,Int) -> Char --line or space in a position in a vertical wall
> vertWallLine maze place | hasWall maze place W = '|'
>                         | otherwise            = ' '

> vertWallEndl :: Maze -> (Int,Int) -> String --space or endl in a position in a vertical wall
> vertWallEndl maze (col,row) | col == x  = "\n" 
>                             | otherwise = "  "
>     where (x,y) = sizeOf maze
