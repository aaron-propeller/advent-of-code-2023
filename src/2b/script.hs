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
          let gamePowers = map gamePower games
          let powerSum = sum gamePowers
          print powerSum


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
gamePower :: Game -> Integer
gamePower (_, outcomes) = do 
  let (maxRed, maxGreen, maxBlue) = maxCubes outcomes (0, 0, 0)
  maxRed * maxGreen * maxBlue

maxCubes :: [Outcome] -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
maxCubes [] gameStatus = gameStatus
maxCubes ((red, green, blue):xs) (currentMaxRed, currentMaxGreen, currentMaxBlue) = do
  let maxRed = max (toNum red) currentMaxRed
  let maxGreen = max (toNum green) currentMaxGreen
  let maxBlue = max (toNum blue) currentMaxBlue
  maxCubes xs (maxRed, maxGreen, maxBlue)


toNum :: Maybe Integer -> Integer
toNum (Just i) = i
toNum Nothing = 0
