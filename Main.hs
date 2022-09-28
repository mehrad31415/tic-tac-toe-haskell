{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}


{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- A handy syntax extension. See:

    http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/syntax-extns.html#tuple-sections

-}
{- 
This program will implement a simple game called Butter, Cheese and Eggs (called by the Dutch).
Generally known as Tic-Tac-Toe or Noughts-and-Crosses (in North America).
-}
module Main where -- Rename to "Main" if you want to compile the game.
                    -- Don't forget to rename it back when submitting!

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.IO

-- | Rose trees

-- a new data type "Rose" has been defined which is a multiway tree data dype. 
-- We have given it the Eq and Show type classes.

data Rose a = MkRose a [Rose a]
    deriving (Eq, Show)

-- Exercise 1

-- the function "root" accepts a "Rose" data type and outputs its root (obsiously!).
-- The root of a tree is the the top node in the hierarchy of the tree.
root :: Rose a -> a
root (MkRose x _) =  x


-- the function "children" accepts a "Rose" data type and outputs the children of the root of the tree.
children :: Rose a -> [Rose a]
children (MkRose _ y) = y

-- Exercise 2

-- counts the number of nodes in a rose tree
-- for a singleton tree the size is 1
-- the size of each tree can be defined as the sum size of its children plus the root node
size :: Rose a -> Int
size (MkRose _ []) = 1
size (MkRose _ xs) = sum (map size xs) + 1
{-
Another method: recursion
size :: Rose a -> Int
size (MkRose x []) = 1
size (MkRose x (y:ys)) = size y + size (MkRose x ys)
-}

-- counts the number of leaves in a rose tree (nodes without any children).
-- the exact implementation as above but we do not want the root node.
leaves :: Rose a -> Int
leaves (MkRose _ []) = 1
leaves (MkRose _ xs) = sum (map leaves xs)

-- | State representation

-- * Players

-- "Player" data type defined with type classes "Eq" and "Ord".
data Player = P1 | P2
    deriving (Eq, Ord)

-- the representation of P1 and P2 when printed on the screen.
instance Show Player where
    show P1 = "Player 1"
    show P2 = "Player 2"

-- Exercise 3

-- given the player whose move it is currently, will return the player who will make a move during the next turn.
nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1


-- * Board

-- data type "Field" defined meaning that each position can be X, O or B (blank).
data Field = X | O | B
    deriving (Eq, Ord)

instance Show Field where
    show X = "X"
    show O = "O"
    show B = " "

-- Exercise 4

-- gives the symbol a particular player uses. (By centuries-old tradition the first player always uses a cross.)
symbol :: Player -> Field
symbol P1 = X
symbol P2 = O

-- defining a Row and Board data type based on Field. (they are syntactic sugars).
type Row   = (Field, Field, Field)
type Board = (Row, Row, Row)

-- Exercise 5

-- gives access to the columns of a board
verticals :: Board -> Board
verticals ( (x1,y1,z1), (x2,y2,z2), (x3,y3,z3) ) = 
          ( (x1,x2,x3), (y1,y2,y3), (z1,z2,z3) )

-- gives access to the diagonals of a board
diagonals :: Board -> (Row, Row)
diagonals  ((x1,_,x3), (_,y2,_), (z1,_,z3)) = ((x1,y2,z3), (x3,y2,z1))

-- Exercise 6

-- an empty board to start the game. (all fields are blank)
emptyBoard :: Board
emptyBoard = ((B, B, B), (B, B, B), (B, B, B))

-- Exercise 7

-- prints the board as a 3 by 3 grid. 
printBoard :: Board -> String
printBoard ((x1,y1,z1) , (x2,y2,z2), (x3,y3,z3)) = 
  show x1 ++ "|" ++ show y1 ++ "|" ++ show z1 ++
  "\n-+-+-\n" ++
  show x2 ++ "|" ++ show y2 ++ "|" ++ show z2 ++
  "\n-+-+-\n" ++
  show x3 ++ "|" ++ show y3 ++ "|" ++ show z3 ++ 
  "\n"

-- | Move generation

-- Exercise 8

-- given a player and a row, returns all possible moves that the player can make in that row
-- the states are divided such that if all fields in a row were blank.
  -- if two fields in a row were blank. 
    -- if only one field of the row was blank.
      -- if none of the fields in that row were blank. (no possible moves can be made)
movesRow :: Player -> Row -> [Row]
movesRow player (x,y,z)
  | x == B && y == B && z == B = 
    [(symbol player, y, z), (x, symbol player, z), (x, y, symbol player)]
  | x == B && y == B = [(symbol player, y, z), (x, symbol player, z)]
  | x == B && z == B = [(symbol player, y, z), (x, y, symbol player)]
  | y == B && z == B = [(x, symbol player, z), (x, y, symbol player)]
  | x == B           = [(symbol player, y ,z)]
  | y == B           = [(x, symbol player ,z)]
  | z == B           = [(x, y ,symbol player)]
  | otherwise        = []

-- given a list of possible rows for row "x" gives a list of the possible boards.
-- take into account that in each turn a player can only make one move so the other rows are unchanged.
combx :: [Row] -> Row -> Row -> [Board]
combx xs y z = map (\ x -> (x, y, z)) xs

-- given a list of possible rows for row "y" gives a list of the possible boards.
comby :: Row -> [Row] -> Row -> [Board]
comby x ys z = map (\ y -> (x, y, z)) ys

-- given a list of possible rows for row "z" gives a list of the possible boards.
combz :: Row -> Row -> [Row]-> [Board]
combz x y zs = map (\ z -> (x, y, z)) zs


-- given the current player and the current state of the board, 
-- returns all possible moves that player can make expressed as a list of resulting boards.
moves :: Player -> Board -> [Board]
moves player (x, y, z) = combx (movesRow player x) y z ++ 
                         comby x (movesRow player y) z ++ 
                         combz x y (movesRow player z)

-- | Gametree generation

-- Exercise 9

-- given a row the function checks whether all fields are equal.
equals :: Row -> Maybe Player
equals (a, b, c)
  | a == b && b == c && c == symbol P1 = Just P1
  | a == b && b == c && c == symbol P2 = Just P2
  | otherwise                          = Nothing

-- given a board, returns which player has won or Nothing if none of the players has won 
-- no players has won can mean two things:
-- 1- the game is still in progress. 2- because it is a draw.
hasWinner :: Board -> Maybe Player 
hasWinner (x, y, z)
  | length (equals x) == 1 = equals x
  | length (equals y) == 1 = equals y
  | length (equals z) == 1 = equals z
  | length (equals a) == 1 = equals a
  | length (equals b) == 1 = equals b
  | length (equals c) == 1 = equals c
  | length (equals m) == 1 = equals m
  | length (equals n) == 1 = equals n
  | otherwise = Nothing
    where (a,b,c) = verticals (x, y, z)
          (m , n) = diagonals (x, y, z)


-- Exercise 10

-- computing the game tree, 
-- if there are no further possible moves for a board the game is finished. 
-- so each board that has a winner or is a draw wihtout a winner is leaf.
-- the game tree of a board will be the board as the root and all possible outcome boards as the children.
gameTree :: Player -> Board -> Rose Board
gameTree p b 
  | length (hasWinner b) == 1 = MkRose b []
  | otherwise                 = gameTree' p b (moves p b)

-- this is a helper function for the gameTree. The following function 
-- is for detecting boards where there are no further boards but no one has won either (there is a draw).
-- (in other words where the (moves p b) list is empty.)
gameTree' :: Player -> Board -> [Board] -> Rose Board
gameTree' p b [] = MkRose b []
gameTree' p b _  = MkRose b  (map (gameTree (nextPlayer p)) (moves p b))

-- | Game complexity

-- Exercise 11

-- given an empty board and P1 starting the game the following constant gives
-- the number of leaves of the gameTree which is also called the complexity of the
-- game tree by game theorists.
gameTreeComplexity :: Int
gameTreeComplexity = leaves (gameTree P1 emptyBoard)

-- | Minimax

-- Exercise 12

-- this is a helper function for the main minimax function. 
-- given a player (whose turn it is) and the next player and a leaf and also the winner of the board
-- it gives MkRose 1 [] for the starter player being the winner 
-- MkRose (-1) [] for the other player being the winner and
-- MkRose (0) [] for draw.
minimax' :: Player -> Player -> Rose Board -> Rose Int
minimax' p _ (MkRose b []) 
  | hasWinner b == Just p              = MkRose 1    []
  | hasWinner b == Just (nextPlayer p) = MkRose (-1) []
  | otherwise                          = MkRose 0    []
minimax' player currentPlayer (MkRose x xs) =  MkRose (minMax (map root ys)) ys
  where ys = map (minimax' player (nextPlayer currentPlayer)) xs
        minMax = if player == currentPlayer then maximum' else minimum'

-- computing the minimax tree for a given player and tree. 
-- here for a leaf we pass the above helper function to see whether a board has winner
-- for other internal nodes in the game tree, there are two options:
-- if player 1 is currently playing, the maximum of the possible future boards will be chosen.
-- otherwise the minimum will be chosen.
minimax :: Player -> Rose Board -> Rose Int
minimax p rb = minimax' p p rb

-- * Lazier minimum and maximums

-- Exercise 13

-- my own defined minimum' and maximum'. The difference is that they stop looking for 
-- a smaller, respectively larger, number in their input list once they encounter the
-- optimum of −1, respectively 1.
minimum' :: [Int] -> Int
minimum' [x] = x
minimum' [x, y] 
  | x <= y = x 
  | otherwise = y
minimum' (x:y:xs)
  | x == -1 || y == -1 = -1
  | x <= y = minimum' (x:xs)
  | otherwise = minimum' (y:xs)


maximum' :: [Int] -> Int
maximum' [x] = x
maximum' [x, y] 
  | x <= y = y 
  | otherwise = x
maximum' (x:y:xs)
  | x == 1 || y == 1 = 1
  | x <= y = maximum' (y:xs)
  | otherwise = maximum' (x:xs)

-- | Gameplay

-- Exercise 14

-- the below helper function is defined for the main makeMove function. 
--given a player and a rose board returns :
-- a board if there are any future possible boards
-- nothing if the MkRose b [] is the input. Because in the game tree these are leaves which dont have any further boards.
-- the way it does this is by checking the values of the minimax tree.
-- specifically if the the number of a minimax tree is equal to one of its children it choses that board.
check :: Player -> Rose Board -> Maybe Board
check p (MkRose b []) = Nothing
check p (MkRose b (y:ys))
  | root (minimax p (gameTree p b)) == root (minimax p (gameTree p (root y))) = Just (root y)
  | otherwise                                                                 = check p (MkRose b ys)
 
-- given a player and a board makes an optimal move
-- (if it is still possible to make a move).
makeMove :: Player -> Board -> Maybe Board
makeMove p b = check p (gameTree p b)
-- | Main

data PlayerType = Human | Computer

instance Show PlayerType where
    show Human    = "H"
    show Computer = "C"

main :: IO ()
main = do
    typeOfP1 <- askFor "Should Player 1 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]
    typeOfP2 <- askFor "Should Player 2 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]

    let playerType :: Player -> PlayerType
        playerType P1 = typeOfP1
        playerType P2 = typeOfP2

        gameLoop :: Player -> Board -> IO ()
        gameLoop p b = do
            putStrLn ("\n" ++ printBoard b)
            case hasWinner b of
                Just p  -> putStrLn (show p ++ " has won!")
                Nothing -> do
                    putStr   ("It's " ++ show p ++ "'s turn. ")
                    mb' <- case playerType p of
                        Human    -> humanMove    p b
                        Computer -> computerMove p b
                    case mb' of
                        Nothing -> do putStr   "No more moves are possible. "
                                      putStrLn "It's a draw."
                        Just b' -> gameLoop (nextPlayer p) b'

        humanMove :: Player -> Board -> IO (Maybe Board)
        humanMove p b =
            case moves p b of
              [] -> return Nothing
              possibleMoves -> do
                putStrLn "Possible moves are:"
                putStrLn (listMoves possibleMoves)
                i <- askFor "Make your choice:" [1..length possibleMoves]
                return (Just (possibleMoves !! (i-1)))

        computerMove :: Player -> Board -> IO (Maybe Board)
        computerMove p b = do
            putStrLn "Thinking..."
            return (makeMove p b)

        listMoves :: [Board] -> String
        listMoves = intercalate "\n"
                    . map (intercalate "    ")
                    . transpose
                    . map lines
                    . map (\(i,b) -> "(" ++ show i ++ "): \n" ++ printBoard b)
                    . zip [1 :: Integer ..]

    gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
    putStr (m ++ " ")
    hFlush stdout
    i <- getLine
    case find ((map toLower i ==) . map toLower . show) xs of
        Nothing -> do putStrLn $ "I didn't understand you. Enter one of: "
                                 ++ intercalate ", " (map show xs) ++ "."
                      askFor m xs
        Just y  -> return y