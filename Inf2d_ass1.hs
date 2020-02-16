-- Inf2d Assignment 1 2018-2019
-- Matriculation number:
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6



{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases.
badNodesList = [(3,3),(2,4)]

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth = 35
-- Why did you choose this number?
-- ANSWER: Since it is a grid consists of 6 times 6 'subgrid', so totally it has 36 'subgrid' which the nodes can be allocated, including the badnodes.
--         So the possible maximum depth is 36 - 1 as the one removed stands for the destination node with the assumption of that there is no badnode, which is that every node in the grid is achievable.


-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

next::Branch -> [Branch]
next [] =  []
next branch
  | null branches = []
  | otherwise     = [node : branches | node <- possiblities, node `notElem` branch, node `notElem` badNodesList, isValid node]
  -- Check whether or not the node have been explored and whether it is badNode
            where
              (x, y) = head branch
              branches = filter (`notElem` badNodesList) branch -- Make sure the initial node will never be the badNode
              possiblities = [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)] -- Provide the possible paths for the current node




-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = destination == curNode


-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch

breadthFirstSearch _ _ [] _ = Nothing
breadthFirstSearch destination next (b : bs) exploredList
  | checkArrival destination (head b) = Just b
  | otherwise = breadthFirstSearch destination next (bs ++ next b) ((head b) : exploredList)


-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.
depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch

depthFirstSearch _ _ [] _ = Nothing
depthFirstSearch destination next (b : bs) exploredList
  | checkArrival destination (head b) = Just b
  | otherwise = depthFirstSearch destination next (next b ++ bs) (head b : exploredList)


-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch
depthLimitedSearch _ _ [] _ = Nothing
depthLimitedSearch destination next (b : bs) depth
  | length b > depth = depthLimitedSearch destination next bs depth
  | checkArrival destination (head b) = Just b
  | otherwise = depthLimitedSearch destination next (next b ++ bs) depth


-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.
iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode d
  | d >= maxDepth = Nothing
  | depthLimitedSearch destination next [[initialNode]] d == Nothing = iterDeepSearch destination next initialNode (d + 1)
    -- Search first in depthLimitedSearch with limit d, if the result not found then do the search with limit d + 1
  | otherwise = depthLimitedSearch destination next [[initialNode]] d


-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
manhattan (xp, yp) (xd, yd) = abs(xp - xd) + abs(yp - yd)
-- Where (xp, yp) is the position point and (xd, yd) is the destination

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic (b : bs) exploredList
  | checkArrival destination (head b) = Just b
  | otherwise = bestFirstSearch destination next heuristic (sortBy sorting (bs ++ next b)) (head b : exploredList)
              where sorting a b | heuristic (head a) > heuristic (head b) = GT
                                | heuristic (head a) < heuristic (head b) = LT
                                | otherwise = EQ
                                -- Sorting function in order to compare the manhatten distance of two positions to the destination




-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost (b : bs) exploredList
  | checkArrival destination (head b) = Just b
  | otherwise = aStarSearch destination next heuristic cost (sortBy sorting (bs ++ next b)) (head b : exploredList)
              where sorting a b | cost a * heuristic (head a) > cost b * heuristic (head b) = GT
                                | cost a * heuristic (head a) < cost b * heuristic (head b) = LT
                                | otherwise = EQ
                                -- Sorting funtion in order to compare the manhattan distance with cost of two positions to the destination


-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost branch = (length branch) - 1
        -- '-1' is to make sure the cost at the origin is 0


-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches.



-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state.
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game | (terminal game) && checkWin game 1 = 1
          | (terminal game) && checkWin game 0 = -1
          | otherwise = 0

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Game->Player->Int
minimax game player | terminal game = eval game
                    | player == 1 = maximum[minimax g (switch player) | g <- (moves game player)]
                    | player == 0 = minimum[minimax g (switch player) | g <- (moves game player)]


-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

alphabeta:: Game->Player->Int
alphabeta game player
  | maxPlayer player = maxValue game (-2) 2
  | otherwise = minValue game (-2) 2
  where
    maxValue game a b
      | terminal game = eval game
      | otherwise = foldl (\v g_ -> if (v >= b) then v else max v $ minValue g_ (max a v) b) a (moves game 1)

    minValue game a b
      | terminal game = eval game
      | otherwise = foldl (\v g_ -> if (v <= a) then v else min v $ maxValue g_ a (min b v)) b (moves game 0)

-- | Section 5.2 Wild Tic Tac Toe





-- | The evalWild function should be used to get the value of a terminal state.
-- It should return 1 if either of the move types is in the correct winning position.
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game | (terminal game) && (checkWin game 1 == True || checkWin game 0 == True) = 1
              | otherwise = 0


-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward.
-- If the min player sent the game into a terminal state you should give -1 reward.

alphabetaWild:: Game->Player->Int
alphabetaWild game player
  | maxPlayer player = maxValue game (-2) 2 player
  | otherwise = minValue game (-2) 2 player
  where
    maxValue game a b player
      | terminal game && maxPlayer player = (evalWild game) * (-1)
      | terminal game = evalWild game
      | otherwise = foldl (\v g_ -> if (v >= b) then v else max v $ minValue g_ (max a v) b $ switch player) a (movesWild game 1)

    minValue game a b player
      | terminal game && maxPlayer player = (evalWild game) * (-1)
      | terminal game = evalWild game
      | otherwise = foldl (\v g_ -> if (v <= a) then v else min v $ maxValue g_ a (min b v) $ switch player) b (movesWild game 0)



-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning).
-- The evalWild function should be used to get the value of a terminal state.

minimaxWild:: Game->Player->Int
minimaxWild game player =undefined



			-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores

isValid :: (Int, Int) -> Bool
isValid (x, y) = x >= 1 && x <= gridLength_search && y >= 1 && y <= gridWidth_search
-- Check the node is in the grid in the next function
