import Data.Char
import GHC.SysTools (isContainedIn)
import Debug.Trace
import GHC.Plugins (wiredInNameTyThing_maybe)
import GHC.Real (reduce)

type Outcome = (Maybe Integer, Maybe Integer, Maybe Integer)
type Game = (Integer, [Outcome])

main :: IO ()
main = do file <- readFile "input.txt"
          let fileLines = lines file
          let games = map parseInput fileLines
          let matchingGames = filter winningGame games
          let gameSum = sum (map fst matchingGames)
          print gameSum


------------------------
-- Game parsing
parseInput :: String -> (Integer, [Outcome])
parseInput s = do let noWhitespace = filter nonSpace s
                  let [gameNumberString, gameRoundsString] = wordsWhen (==':') noWhitespace
                  let gameNumber = extractNumber gameNumberString
                  let gameResults = parseAllRounds gameRoundsString
                  (gameNumber, gameResults) 

parseAllRounds :: String -> [Outcome]
parseAllRounds s = do let roundStrings = wordsWhen (==';') s
                      map parseRound roundStrings

parseRound :: String -> Outcome
parseRound s = do let xs = wordsWhen (==',') s
                  let red = extractColour xs "red"
                  let blue = extractColour xs "blue"
                  let green = extractColour xs "green"
                  (red, green, blue)

extractColour :: [String] -> String -> Maybe Integer
extractColour [] colour = Nothing
extractColour (x:xs) colour = do
    if isContainedIn colour x then Just (extractNumber x)
    else extractColour xs colour

-- Split string at Char
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                    where (w, s'') = break p s'

-- extract all numbers from a string
extractNumber :: String -> Integer
extractNumber s = read (filter isDigit s) :: Integer

nonSpace :: Char -> Bool
nonSpace = not . isSpace
-------------------------
--
winningGame :: Game -> Bool
winningGame (_, outcomes) = winningOutcome outcomes True

winningOutcome :: [Outcome] -> Bool -> Bool
winningOutcome [] gameStatus = gameStatus
winningOutcome ((red, green, blue):xs) gameStatus = do
  let redWin = redWithinLimits red
  let greenWin = greenWithinLimits green
  let blueWin = blueWithinLimits blue
  winningOutcome xs (gameStatus && redWin && greenWin && blueWin)

redWithinLimits :: Maybe Integer -> Bool
redWithinLimits (Just i) = i <= 12
redWithinLimits Nothing = True

greenWithinLimits :: Maybe Integer -> Bool
greenWithinLimits (Just i) = i <= 13
greenWithinLimits Nothing = True

blueWithinLimits :: Maybe Integer -> Bool
blueWithinLimits (Just i) = i <= 14
blueWithinLimits Nothing = True
