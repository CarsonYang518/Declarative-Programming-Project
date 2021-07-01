-------------------------------------------------------------------------
-- Author: Kaixun Yang <kaixuny@student.unimelb.edu.au>
-- ID: 1040203
-- Purpose: A guessing cards game in Haskell
-- 
-- There are two players in this game, each with a complete standard 
-- deck of western playing cards (without jokers), one of them will
-- select some cards from the deck (2 to 4 cards) without showing the 
-- other player, the other player needs to guess what these cards are. 
-- Guessing players can get feedback message every time he/she guesses 
-- and need to rely on the feedback to make new guesses until the guess 
-- is correct. We need to reduce the number of guesses.
-- 
-- Strategy:
-- 1. Generate a list containing all the combinations of different cards, 
-- and get the cards of different suits with an interval of about 13/(n+1) 
-- as first guess.
-- 2. According to the feedback value, only keep the combinations of 
-- cards in the list that can get the same feedback value with pervious guess
-- 3. If the number of remaining combinations is larger than 1326, it will 
-- directly choose the middel one of the remaining combinations as a new guess. 
-- Otherwise, it will choose the guess that is likely to leave the smallest 
-- remaining list of possible combinations.
-- 4. It will executes step 2 and 3 iteratively until the guess is equal to 
-- the answer.
-- Date: 14/05/2021
-------------------------------------------------------------------------
module Proj2 (feedback, initialGuess, nextGuess, GameState) where

import Card

import Data.List

-------------------------------------------------------------------------
-------------------------Type declaration--------------------------------

-- Type for the answer and guess, a list of cards.
type Guess = [Card]

-- Type for GameState, which is a list of lists of cards, representing the 
-- remaining selectable combinations of cards.
type GameState = [Guess]

-- Type for Feedback, which is a tuple of five intergers, representing
-- number of correct cards, number of answer ranks below the lowest rank in the 
-- guess, number of correct ranks, number of answer ranks above the highest rank 
-- in the guess and number of correct suits.
type Feedbcak = (Int,Int,Int,Int,Int)

-------------------------------------------------------------------------

-------------------------------------------------------------------------
-------------------------Get initial guess-------------------------------

-- Generate the first guess and the list of all possible combinations of 
-- different cards.

-- Input: The number of cards.
-- Output: A tuple contains the first guess of cards and all possible 
-- combinations of different cards.
--
-- Use list comprehension to generate the first guessed with n cards.
-- Choose cards with ranks that are about 13/(n+1) ranks apart and 
-- differnt suits. Use toEnum in class Enum to get the value of data with 
-- a specific index(start with 0). Use [minBound..maxBound]::[Card] in class 
-- Bounded to get a list of all cards. Call getAllGuesses to get the GameState 
-- of all possible combinations of different cards.
initialGuess :: Int -> (Guess, GameState)
initialGuess n = ([Card (toEnum $ (i-1) `mod` 4) (toEnum $ (i * gap) `mod` 13) 
                 | i <- [1..n]], getAllGuesses n cards) 
                     where gap = 13 `div` (n + 1)
                           cards = [minBound..maxBound]::[Card]

-- Input: The number of cards and a list of all cards.
-- Output: A list of all possible combinations of different cards.
--
-- A helper function. 
-- Use recursion and map to generate all possible combinations of different 
-- cards.
getAllGuesses :: Int -> [Card] -> [Guess]
getAllGuesses 0 _ = [[]]
getAllGuesses _ [] = []
getAllGuesses n (x:xs) = map (x:) (getAllGuesses (n-1) xs) ++ getAllGuesses n xs

-------------------------------------------------------------------------

-------------------------------------------------------------------------
-------------------------Get feedback value------------------------------

-- Get the feedback value after a guess.

-- Input: The list of answer cards and the list of guess cards.
-- Output: The Feedback tuple contains five integer, representing
-- number of correct cards, number of answer ranks below the lowest rank in the 
-- guess, number of correct ranks, number of answer ranks above the highest rank 
-- in the guess and number of correct suits.
--
-- The main function. Call getRanks to get the list of ranks of answer and 
-- guess. Call getSuits to get the list of suits of answer and guess. 
-- Use minimum and maximum to get the min rank and max rank in guess.
-- Use length and call getCorretCard and getIntersection to 
-- get number of correctCards, correctRanks and correctSuits.
-- Use length and filter get the number of lowerRanks and higherRanks.
feedback :: Guess -> Guess -> Feedbcak
feedback answer guess = (correctCards, lowerRanks, correctRanks, 
    higherRanks, correctSuits)
    where answerSuits = getSuits answer
          answerRanks = getRanks answer
          guessSuits = getSuits guess
          guessRanks = getRanks guess
          lowerRank = minimum guessRanks
          higherRank = maximum guessRanks
          correctCards = length $ getCorretCard answer guess
          correctRanks = length $ getIntersection answerRanks guessRanks
          correctSuits = length $ getIntersection answerSuits guessSuits
          lowerRanks = length $ filter (<lowerRank) answerRanks
          higherRanks = length $ filter (>higherRank) answerRanks

-- Input: A list of cards (A guess).
-- Output: A list of suits of the guess.
--
-- A helper function. Use map and suit to get all suits of cards in the guess.
getSuits :: Guess -> [Suit]
getSuits guess = map suit guess

-- Input: A list of cards (A guess).
-- Output: A list of ranks of the guess.
--
-- A helper function. Use map and rank to get all ranks of cards in the guess.
getRanks :: Guess -> [Rank]
getRanks guess = map rank guess

-- Input: The list of answer cards and the list of guess cards.
-- Output: The list of correct guessed cards.
--
-- A helper function. Use list comprehension to generate the list of 
-- correct guessed cards.
getCorretCard ::  Guess -> Guess -> Guess
getCorretCard answer guess = [answerCard | answerCard <- answer, 
                             guessCard <- guess, answerCard == guessCard]

-- Input: Two list.
-- Output: The Intersection of two list.
--
-- A helper function. Use \\ to deletes each of the elements of one list 
-- from another list.
-- Use two times \\ to get the Intersection of two list.
getIntersection :: Eq a => [a] -> [a] -> [a]
getIntersection xs ys = xs \\ (xs \\ ys)

-------------------------------------------------------------------------

-------------------------------------------------------------------------
-------------------------Get next guess----------------------------------

-- Get the next guess depend on pervious guess, feedback and GameState.
-- Update the GameState to get remaining possible combinations of different 
-- cards.

-- Input: A tuple containing a list of guess cards and a list of possible 
-- combinations of different cards, a tuple of feedback value.
-- Output: A tuple containing a list of new guess cards and a list of 
-- remaining possible combinations of different cards
--
-- The main function. Call getRemainGuesses to get the remaining possible 
-- combinations of different cards.
-- Use !! to choose the middel one of the remaining combinations as a new 
-- guess if the number of remaining combinations is larger than 1326. Because 
-- too many calculations will cause timeout.
-- Otherwise, it will call getBestGuess to get the guess that is likely 
-- to leave the smallest remaining list of possible combinations.
nextGuess :: (Guess, GameState) -> Feedbcak -> (Guess, GameState)
nextGuess (guess, allGuesses) feedbackValue = (newGuess, newAllGuesses)
    where newAllGuesses = getRemainGuesses guess allGuesses feedbackValue
          newAllGuessesLength = length newAllGuesses
          newGuess = 
              if newAllGuessesLength > 1326
              then newAllGuesses !! (newAllGuessesLength `div` 2)
              else getBestGuess newAllGuesses

-- Input: A list of guess cards, a list of possible combinations of different 
-- cards, a tuple of feedback value.
-- Output: A list of remaining possible combinations of different cards.
--
-- A helper function. Use filer to get all combinations of different cards that 
-- can get the same feedback value with pervious guess. Use . to chain two 
--function. Use flip to change the sequence of arguments in a function.
getRemainGuesses :: Guess -> GameState -> Feedbcak -> GameState
getRemainGuesses guess allGuesses feedbackValue = 
    filter ((==feedbackValue) . flip feedback guess) allGuesses

-- Input: A list of possible combinations of different cards.
-- Output: A list of guess cards.
--
-- A helper function. Use list comprehension to get a list of tuples containing 
-- a guess and all possible combinations of different cards.
-- Use list comprehension, group, sort and call getFeedbacks to get a list of 
-- tuples containing the guess and the grouped feedback value if we choose the 
-- guess. Use list comprehension and call getExpectedNumber to get a list of 
-- tuples containing the average number of possible answers it will leave if we 
-- choose the guess and the guess. Use head and sort to get the guess which 
-- leads to the min number of possible answers.
getBestGuess :: GameState -> Guess
getBestGuess allGuesses = newGuess
    where 
          guessAllGuessesPairs = [(guess, allGuesses) 
              | guess <- allGuesses] 
          guessFeedbackGroupPairs = 
              [(guess, group $ sort $ getFeedbacks guess allGuesses) 
              | (guess, allGuesses) <- guessAllGuessesPairs]
          guessExpectedNumberPairs = 
              [(getExpectedNumber groupedFeedbacks, guess) 
              | (guess, groupedFeedbacks) <- guessFeedbackGroupPairs]
          (_, newGuess) = head $ sort guessExpectedNumberPairs

-- Input: A list of Grouped feedback (a list).
-- Output: An integer shows the average number of possible answers it will leave 
-- if we guess it.
--
-- A helper function. Use sum, map and length get the sum of the group sizes.
-- Use . to combine two function length and sqare, and use sum and map to get 
-- the sum of the squares of the group sizes.
-- Get output by sum of the squares of the group sizes divided by the sum of 
-- the group sizes. 
getExpectedNumber :: [[Feedbcak]] -> Int
getExpectedNumber groupedFeedbacks = squareSumGroupSize `div` sumGroupSize
    where squareSumGroupSize = sum $ map ((^2) . length) groupedFeedbacks
          sumGroupSize = sum $ map length groupedFeedbacks

-- Input: A list of guess cards, a list of possible combinations of different. 
-- Output: A list of feedback, representing the feedback you will receive for 
-- each possible answer A if G is the guess and A is the answer. 
--
-- A helper function. Use flip to change the sequence of arguments in feedback.
-- Use map to get the feedback list.
getFeedbacks :: Guess -> [Guess] -> [Feedbcak]
getFeedbacks guess allGuesses = map (flip feedback guess) allGuesses

-------------------------------------------------------------------------