{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveTraversable #-}

module Options where

import Data.List (foldl')
import Paths_wordle
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO

data Options a = Options
  { optDictionary :: a,
    optWordList :: a,
    optStrategy :: Strategy,
    optMode :: Mode,
    optHard :: Bool,
    optKeyboard :: Keyboard
  }
  deriving (Read, Show, Eq, Ord, Foldable, Functor, Traversable)

data Keyboard
  = Qwerty
  | Dvorak
  | Colemak
  | Alphabet
  | Frequencies
  deriving (Read, Show, Eq, Ord)

data Strategy
  = WorstCase
  | MaxEntropy
  | SumOfSquares
  | MostChoices
  deriving (Read, Show, Eq, Ord)

data Mode
  = Play
  | Give
  | Solve [String]
  deriving (Read, Show, Eq, Ord)

defaultOptions :: Options FilePath
defaultOptions =
  Options
    { optDictionary = "all.txt",
      optWordList = "play.txt",
      optStrategy = MostChoices,
      optMode = error "defaultOptions: mode not set",
      optHard = False,
      optKeyboard = Qwerty
    }

optDescriptions :: [OptDescr (Options FilePath -> Options FilePath)]
optDescriptions =
  [ Option [] ["dict"] (ReqArg (\x o -> o {optDictionary = x}) "FILE") "Dictionary",
    Option [] ["words"] (ReqArg (\x o -> o {optWordList = x}) "FILE") "Word list",
    Option [] ["worstCase"] (NoArg \o -> o {optStrategy = WorstCase}) "Strategy: worst case",
    Option [] ["maxentropy"] (NoArg \o -> o {optStrategy = MaxEntropy}) "Strategy: maximum entropy",
    Option [] ["sumofsquares"] (NoArg \o -> o {optStrategy = SumOfSquares}) "Strategy: sum of squares",
    Option [] ["mostchoices"] (NoArg \o -> o {optStrategy = MostChoices}) "Strategy: most choices (default)",
    Option [] ["easy"] (NoArg \o -> o {optHard = False}) "Disable hard mode (default)",
    Option [] ["hard"] (NoArg \o -> o {optHard = True}) "Enable hard mode",
    Option [] ["qwerty"] (NoArg \o -> o {optKeyboard = Qwerty}) "Keyboard layout: qwerty (default)",
    Option [] ["dvorak"] (NoArg \o -> o {optKeyboard = Dvorak}) "Keyboard layout: dvorak",
    Option [] ["colemak"] (NoArg \o -> o {optKeyboard = Colemak}) "Keyboard layout: colemak",
    Option [] ["alphabet"] (NoArg \o -> o {optKeyboard = Alphabet}) "Keyboard layout: alphabet",
    Option [] ["frequencies"] (NoArg \o -> o {optKeyboard = Frequencies}) "Keyboard layout: frequencies"
  ]

usage :: IO a
usage = do
  hPutStr stderr (usageInfo header optDescriptions)
  exitFailure
  where
    header =
      "Usage: wordle [FLAGS] MODE\n\
      \\n\
      \Modes:\n\
      \    solve - Program guesses a secret word, reply with 'b' 'y' 'g'\n\
      \    play  - Program picks a random word, type in your guess words\n\
      \    give  - User types in the secret words, then types in quess words\n"

getOptions :: IO (Options [String])
getOptions = do
  args <- getArgs

  dictPath <- getDataFileName "all.txt"
  wordsPath <- getDataFileName "play.txt"

  let o0 m =
        defaultOptions
          { optDictionary = dictPath,
            optWordList = wordsPath,
            optMode = m
          }

  case getOpt Permute optDescriptions args of
    (_, _, es) | not (null es) ->
      do
        mapM_ (hPutStrLn stderr) es
        usage
    (fs, ms, _) ->
      do
        let opts m = foldl' (\x f -> f x) (o0 m) fs
        opts' <- case ms of
          "solve" : start -> pure (opts (Solve start))
          ["play"] -> pure (opts Play)
          ["give"] -> pure (opts Give)
          _ -> usage
        traverse (fmap lines . readFile) opts'
