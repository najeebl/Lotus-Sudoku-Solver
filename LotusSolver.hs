{-
Name: Najeeb Lakhani and Neil Dunn
Class: EECS 368 - Programming Language Paradigms
Date Last Modified: Tuesday May 3rd 4:23 PM
Project 2 - Lotus Sudoku Solver 
This program takes in an List as an Input and returns a solved board.
-}


import Data.List.Split
import Data.List

{-\
The boards we are solving.
All boards are list of ints.
The program will solve all 6 given boards.
-}

unsolvedboard1::[Int]
unsolvedboard1 = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0]

unsolvedboard2::[Int]
unsolvedboard2 = [4,1,2,3,6,0,0,0,0,0,0,1,0,0,0,1,7,4,0,0,2,0,0,0,0,1,0,5,3,0,0,4,0,0,0,0,7,0,0,0,0,0,0,1,2,0,0,0,0]  

unsolvedboard3::[Int]
unsolvedboard3 = [0,1,0,7,6,0,0,4,0,0,1,0,0,0,0,0,6,0,0,5,0,0,0,0,0,0,0,5,0,0,0,0,0,2,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0]

unsolvedboard4::[Int]
unsolvedboard4 = [4,0,5,3,0,1,7,1,7,0,0,0,0,0,0,0,6,0,0,5,2,1,2,3,0,0,0,5,6,0,7,4,0,1,3,0,0,0,0,0,0,0,1,4,0,6,0,7,0]

unsolvedboard5::[Int]
unsolvedboard5 = [0,1,2,0,6,0,0,0,0,7,1,0,0,0,0,0,6,0,0,0,0,1,0,0,0,0,0,0,6,0,0,0,0,2,0,2,3,0,0,6,0,0,1,4,0,0,0,0,0]

unsolvedboard6::[Int]
unsolvedboard6 = [0,0,0,7,6,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0]

{-\
Inputs: List
Output: 2D List
Breaks the board to a double dimensional List
-}
chunkBoard :: [Int] -> [[Int]]
chunkBoard b = chunksOf 7 b


{-\
Inputs: Int -> Int -> 2D List
Output: List
Takes the starting coordinate at the top of the board and returns the left arc
-}
getLeftArc :: Int -> Int -> [[Int]] -> [Int]
getLeftArc x y board
    | (y == 6) =((board !! y)!! (mod(x+3) 7)):[]
	| (y == 0 || y == 1) = ( (board !! y)!!(mod x 7)):getLeftArc x (y+1) board
	| (y == 2 || y == 3) = ( (board !! y)!!(mod(x+1) 7)):getLeftArc x (y+1) board
	| (y == 4 || y == 5) = ( (board !! y)!!(mod(x+2) 7)):getLeftArc x (y+1) board


{-\
Inputs: Int -> Int -> 2D List
Output: List
Takes the starting coordinate at the top of the board and returns the right arc
-}
getRightArc :: Int -> Int -> [[Int]] -> [Int]
getRightArc x y board
	| (y == 5) =((board !! y)!! (mod(x-3) 7)):getRightArc x (y+1) board
	| (y == 6) = ((board !! y)!! (mod(x-3) 7)):[]
	| (y == 0) = ( (board !! y)!!(mod x 7)):getRightArc x (y+1) board
	| (y == 1 || y == 2) = ( (board !! y)!!(mod(x-1) 7)):getRightArc x (y+1) board
	| (y == 3 || y == 4) = ( (board !! y)!!(mod(x-2) 7)):getRightArc x (y+1) board


{-\
Inputs: 2D List -> Int -> Int -> Int
Output: 2D List
Takes the coordinate at which the value needs to be placed and returns a
board with the new value in place
-}
placeValue :: [[Int]] -> Int -> Int -> Int -> [[Int]]
placeValue board change rowy colx = take rowy board ++ [take colx (board !! rowy) ++ [change] ++ drop (colx + 1) (board !! rowy)] ++ drop (rowy + 1) board


{-\
Inputs: 2D List
Output: 2D List
Returns a 2D List of all the Arcs on the board that will be checked in solvedBoard
and in the solve functions
-}
getArcs::[[Int]] -> [[Int]]
getArcs board = allArcs
    where
        allArcs = rightArcZero:rightArcOne
                  :rightArcTwo:rightArcThree
                  :rightArcFour:rightArcFive
                  :rightArcSix:leftArcZero
                  :leftArcOne:leftArcTwo
                  :leftArcThree:leftArcFour
                  :leftArcFive:leftArcSix:[]
            where
                rightArcZero  = (getRightArc 0 0 (board))
                rightArcOne   = (getRightArc 1 0 (board))
                rightArcTwo   = (getRightArc 2 0 (board))
                rightArcThree = (getRightArc 3 0 (board))
                rightArcFour  = (getRightArc 4 0 (board))
                rightArcFive  = (getRightArc 5 0 (board))
                rightArcSix   = (getRightArc 6 0 (board))
                leftArcZero   = (getLeftArc  0 0 (board))
                leftArcOne    = (getLeftArc  1 0 (board))
                leftArcTwo    = (getLeftArc  2 0 (board))
                leftArcThree  = (getLeftArc  3 0 (board))
                leftArcFour   = (getLeftArc  4 0 (board))
                leftArcFive   = (getLeftArc  5 0 (board))
                leftArcSix    = (getLeftArc  6 0 (board))


{-\
Input: List
Output: Bool
Takes in a List and makes sure the list has values between 1-7  and all values
occur only once in the list. Helper function for solvedBoard
-}
solvedBoardMapper:: [Int] -> Bool
solvedBoardMapper list = ((all (>0) list)
                         && (all (<8) list))
                         && atMostOnce 1 list
                         && atMostOnce 2 list
                         && atMostOnce 3 list
                         && atMostOnce 4 list
                         && atMostOnce 5 list
                         && atMostOnce 6 list
                         && atMostOnce 7 list


{-\
Inputs: 2D List
Output: Bool
This function takes the board and all the all the Arcs and then maps the
solvedBoardMapper to make sure the board is a valid solution
-}
solvedBoard::[[Int]] -> Bool
solvedBoard board
    | all (==True) ((map solvedBoardMapper board) ++ (map solvedBoardMapper (getArcs (board)))) = True
    | otherwise = False

{-\
Inputs: Int -> List
Output: Bool
Takes the list and the Int and uses the filter function to make sure that the
number occurs either once or doesn't occur at all. This is a helper function for solvedBoardMapper and
checkForDuplicates. 
-}
atMostOnce :: Int -> [Int] -> Bool
atMostOnce x list
	| (length ((filter (==x)) list) == 1) = True
	| (length ((filter (==x)) list) == 0) = True
	| (length ((filter (==x)) list) == 1) && (length ((filter (==0)) list) > 0)= False
	| (length ((filter (==x)) list) == 0) && (length ((filter (==0)) list) > 0) = False
	| otherwise = False


{-\
Inputs: List
Output: Bool
Takes in the List and makes sure the each value occurs only once. Helper function
for solve.
-}
checkForDuplicates:: [Int] -> Bool
checkForDuplicates list =  atMostOnce 1 list
                           && atMostOnce 2 list
                           && atMostOnce 3 list
                           && atMostOnce 4 list
                           && atMostOnce 5 list
                           && atMostOnce 6 list
                           && atMostOnce 7 list


{-\
Inputs: 2D List -> Int -> Int
Output: 2D List
Takes in a board and takes X Y cooridnate on the 2d representation of the board.
If the board is valid it return the solved board.
If the X cooridate is at the end of the column, it goes to next row.
If the value is already filled it will call solve and pass down that value and move to the next coordinate.
If all the above condtions don't occur it will test if the numbers between 1-7 can be placed.
If the the value can be placed, it will call solve on the board with the possible value.
If the value failes it will back out and test another value.
-}
solve::[[Int]] -> Int -> Int  -> [[Int]]
solve board rowy colx
    | placementChecker board == False = board
	| (solvedBoard board == True) = board
	| (((board!!rowy)!!colx) /= 0 && (colx == 6)) = (solve (placeValue board (board!!rowy!!colx) rowy colx ) (rowy + 1) (0))
	| (((board!!rowy)!!colx) /= 0) = (solve (placeValue board (board!!rowy!!colx) rowy colx ) rowy (colx + 1))
    | ((((board!!rowy)!!colx) == 0) &&  placementChecker (placeValue board 1 rowy colx) && placementChecker (solvedWith1)) = (solvedWith1)
    | ((((board!!rowy)!!colx) == 0) &&  placementChecker (placeValue board 2 rowy colx) && placementChecker (solvedWith2)) = (solvedWith2)
    | ((((board!!rowy)!!colx) == 0) &&  placementChecker (placeValue board 3 rowy colx) && placementChecker (solvedWith3)) = (solvedWith3)
    | ((((board!!rowy)!!colx) == 0) &&  placementChecker (placeValue board 4 rowy colx) && placementChecker (solvedWith4)) = (solvedWith4)
    | ((((board!!rowy)!!colx) == 0) &&  placementChecker (placeValue board 5 rowy colx) && placementChecker (solvedWith5)) = (solvedWith5)
    | ((((board!!rowy)!!colx) == 0) &&  placementChecker (placeValue board 6 rowy colx) && placementChecker (solvedWith6)) = (solvedWith6)
    | ((((board!!rowy)!!colx) == 0) &&  placementChecker (placeValue board 7 rowy colx) && placementChecker (solvedWith7)) = (solvedWith7)
    | otherwise = []
        where
            solvedWith1 = (solve (placeValue board 1 rowy colx ) rowy (mod(colx + 1) 6))
            solvedWith2 = (solve (placeValue board 2 rowy colx ) rowy (mod(colx + 1) 6))
            solvedWith3 = (solve (placeValue board 3 rowy colx ) rowy (mod(colx + 1) 6))
            solvedWith4 = (solve (placeValue board 4 rowy colx ) rowy (mod(colx + 1) 6))
            solvedWith5 = (solve (placeValue board 5 rowy colx ) rowy (mod(colx + 1) 6))
            solvedWith6 = (solve (placeValue board 6 rowy colx ) rowy (mod(colx + 1) 6))
            solvedWith7 = (solve (placeValue board 7 rowy colx ) rowy (mod(colx + 1) 6))


{-
This function is called to see if the placement of the new value will result in
a valid board that can be passed on to be solve by the recursive solve function.
-}
placementChecker::[[Int]] -> Bool
placementChecker [] = False
placementChecker board = (((all (==True) (map checkForDuplicates board)) == True) && (all (==True) (map checkForDuplicates (getArcs board)) == True))

{-\
Inputs: List
Output: List
Takes in a board, breaks it down into a 2D array and calls solve on it.
Then combines the resulting list.
-}
lotusSolver:: [Int] -> [Int]
lotusSolver board =  intercalate [] (solve (chunkBoard board) 0 0)

--Solves given boards
main = do 
	putStrLn $ "Given:    " ++ show( unsolvedboard1) ++ "\n" ++ "Solution: " ++ show(lotusSolver unsolvedboard1 ) ++ "\n" 
	putStrLn $ "Given:    " ++ show( unsolvedboard2) ++ "\n" ++ "Solution: " ++ show(lotusSolver unsolvedboard2 ) ++ "\n" 
	putStrLn $ "Given:    " ++ show( unsolvedboard3) ++ "\n" ++ "Solution: " ++ show(lotusSolver unsolvedboard3 ) ++ "\n" 
	putStrLn $ "Given:    " ++ show( unsolvedboard4) ++ "\n" ++ "Solution: " ++ show(lotusSolver unsolvedboard4 ) ++ "\n" 
	putStrLn $ "Given:    " ++ show( unsolvedboard5) ++ "\n" ++ "Solution: " ++ show(lotusSolver unsolvedboard5 ) ++ "\n"
	putStrLn $ "Given:    " ++ show( unsolvedboard6) ++ "\n" ++ "Solution: " ++ show(lotusSolver unsolvedboard6 ) ++ "\n" 


	
	
	
	
	
