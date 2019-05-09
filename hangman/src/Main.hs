module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)


------------------------------------------------------------------
type WordList = [String]

allWords :: IO WordList
allWords = lines <$> readFile "data/words.txt"

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int 
maxWordLength = 9


gameWords :: IO WordList
gameWords = filter isWithinLength <$> allWords
    where
        isWithinLength w = let l = length w in 
                                           l >= minWordLength
                                           && l < maxWordLength


randomWord :: WordList -> IO String
randomWord wl = (!!) wl <$> randomRIO (22, 3333)


randomWord' :: IO String
randomWord' = gameWords >>= randomWord

------------------------------------------------------------------
data Puzzle = Puzzle String [Maybe Char] String


instance Show Puzzle where 
    show (Puzzle _ discovered guessed) =
            (intersperse ' ' . fmap renderPuzzleChar) discovered 
            <> " Guessed so far: " <> guessed


freshPuzzle :: String -> Puzzle
freshPuzzle str = 
    Puzzle str nothingStr "" 
        where nothingStr = fmap (const Nothing) str


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _) c = c `elem` str


alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = c `elem` g


renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing   =  '_'
renderPuzzleChar (Just c)  =  c


fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
    Puzzle word newFilledInSoFar (c : s)
        where 
            zipper guessed wordChar guessChar =
                if wordChar == guessed then Just wordChar else guessChar
            newFilledInSoFar = zipWith (zipper c) word filledInSoFar


handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your gess was: " <> [guess]

    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
      (_, True)  ->  do
          putStrLn "You already guessed that character, pick something else!"
          return puzzle
      (True, _)  ->  do
          putStrLn "This character was in the word, filling in the word accordingly"
          return $ fillInCharacter puzzle guess
      (False, _) -> do
          putStrLn "This character wasn't in the word, try again."
          return $ fillInCharacter puzzle guess


gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) 
  | length guessed <= 7  =  return ()
  | otherwise            =  do
      putStrLn "You lose!"
      putStrLn $ "The word was: " <>  wordToGuess


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _)  
    | all isJust filledInSoFar  =  putStrLn "You win!" >> exitSuccess
    | otherwise                 =  return ()


runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle

    putStrLn $ "Current puzzle is: " <> show puzzle
    putStr "Guss a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _   -> putStrLn "Your guess must be a single character"


------------------------------------------------------------------
main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle
