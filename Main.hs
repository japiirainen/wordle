module Main (main) where

import Control.Monad
import qualified Data.Map as Map
import Options
import System.Console.ANSI
import System.IO
import System.Random (randomRIO)

main :: IO ()
main = do
  opts <- getOptions
  case optMode opts of
    Solve xs -> print $ "solve" <> show xs
    Give -> print "give"
    Play -> withCursor $ play opts

withCursor :: IO c -> IO c
withCursor m = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen
  setCursorPosition 0 0
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Magenta]
  putStrLn "[ W O R D L E ]"
  m

-- | Play wordle with a ramdomly-selected word
play :: Options [String] -> IO ()
play opts = do
  w <- randomFromList $ optWordList opts
  playLoop opts (optDictionary opts) w Nothing Map.empty

playLoop ::
  Options [String] ->
  [String] ->
  String ->
  Maybe [(Clue, Char)] ->
  Map.Map Char Clue ->
  IO ()
playLoop opts remain answer prev letters = do
  printLetters (optKeyboard opts) letters
  w <- getWord opts prev remain
  let clue = computeClues answer w
      remain' = filter (\x -> computeClues x w == clue) remain
      letters' = Map.unionWith max letters (Map.fromListWith max (zip w clue))
  putStrLn ('\r' : prettyWord (zip (Just <$> clue) w))
  unless
    (clue == replicate 5 Hit)
    (playLoop opts remain' answer (Just (zip clue w)) letters')

-- | Render a word with colors indicating clue status
prettyWord :: [(Maybe Clue, Char)] -> String
prettyWord xs = colorWord [(maybe Blue clueColor c, x) | (c, x) <- xs]

clueColor :: Clue -> Color
clueColor Hit = Green
clueColor Miss = Black
clueColor Near = Yellow

colorWord :: [(Color, Char)] -> String
colorWord w =
  setSGRCode [SetConsoleIntensity BoldIntensity]
    <> foldr f (setSGRCode [Reset]) w
  where
    f (x, y) z =
      setSGRCode [SetColor Background Dull x, SetColor Foreground Dull White]
        <> [' ', y, ' ']
        <> z

data Clue
  = -- | Letter is not in the word, or is covered by an earlier diplicat
    Miss
  | -- | Letter is in word, but not at this position
    Near
  | -- | Letter is in the word at this position
    Hit
  deriving (Eq, Ord, Read, Show)

-- * List utilities

-- | Uniform selection of an element from a list.
randomFromList :: [a] -> IO a
randomFromList xs
  | null xs = fail "randomFromLsit: empty list"
  | otherwise = do
    i <- randomRIO (0, length xs - 1)
    pure $! xs !! i

-- | Biased choice; return the first list unless it's empty.
(<++) :: [a] -> [a] -> [a]
[] <++ xs = xs
xs <++ _ = xs
