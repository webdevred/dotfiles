{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

import Data.Aeson (encode)
import Data.Aeson.Types (Parser)
import Data.Char (isDigit)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Lazy as TL (strip, unpack)
import qualified Data.Text.Lazy.Encoding as TLE (decodeUtf8)
import GHC.Stack (withFrozenCallStack)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.Process (readCreateProcess, shell)
import Text.Printf (printf)
import Text.Read (readEither)

data Monitor =
  Monitor
    { monitorName :: String
    , monitorId :: Int
    }
  deriving (Show)

type Bar = String

parseMonitor :: String -> Monitor
parseMonitor monStr = Monitor parseMonitorName parseMonitorId
  where
    parseMonitorId = read $ takeWhile (/= ':') $ dropWhile (== ' ') monStr
    parseMonitorName = reverse $ takeWhile (/= ' ') (reverse monStr)

listBars :: IO [Bar]
listBars = filter (isSuffixOf ".hs") <$> getDirectoryContents "xmobar"

listMonitors :: IO [Monitor]
listMonitors = do
  output <- readCreateProcess (shell "xrandr --listmonitors") ""
  let monitors = map parseMonitor . drop 1 . lines $ output
   in return monitors

printMonitor :: Monitor -> IO ()
printMonitor mon = putStrLn $ "Processing monitor: " ++ monitorName mon

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy delimiterChar inputString = foldr f [""] inputString
  where
    f :: Char -> [String] -> [String]
    f currentChar allStrings@(partialString:handledStrings)
      | currentChar == delimiterChar = "" : allStrings
      | otherwise = (currentChar : partialString) : handledStrings

readBarIndex :: Int -> String -> Either String Int
readBarIndex i input
  | null input = Left "input is empty"
  | all isDigit input =
    case readEither input of
      Right b ->
        if b < i
          then Right b
          else Left $ outOfBoundMsg b
      Left err -> Left $ "unexpected parse error: " ++ err
  | otherwise = Left $ "invalid input (only digits allowed): " ++ show input
  where
    outOfBoundMsg b =
      "bar index out of bounds: " ++
      show b ++ ", only (0- " ++ show (i - 1) ++ ")"

type MonitorState = (String, [String])

updateMonitor :: [Bar] -> String -> MonitorState -> IO MonitorState
updateMonitor bars bs (monId, barAcc) =
  let barIndexesEither = mapM (readBarIndex $ length bars) . splitBy ',' $ bs
   in case barIndexesEither of
        Right barIndexes -> return (monId, barAcc ++ map (bars !!) barIndexes)
        Left err -> do
          putStrLn $ "error: " ++ err
          return (show monId, barAcc)

prompt :: String -> Int -> String
prompt _ 0 = ""
prompt nbars 1 = "Add new bar or exit? (0-" ++ nbars ++ " or c)"
prompt nbars 2 =
  "Add new bar or continue to last monitor? (0-" ++ nbars ++ " or c) "
prompt nbars 3 =
  "Add new bar or continue to next monitor? (0-" ++ nbars ++ " or c)"

configureBars ::
     Monitor -> Int -> Int -> [Bar] -> MonitorState -> IO MonitorState
configureBars mon nmons nbars bars state = do
  _ <- putStrLn $ prompt (show $ nbars - 1) $ nmons - monitorId mon
  bs <- getLine
  if not $ isPrefixOf bs "continue"
    then do
      newState <- updateMonitor bars bs state
      putStrLn $ "Current bars on monitor " ++ monName ++ ": " ++ joinBars newState
      configureBars mon nmons nbars bars newState
    else return state
  where
    monName = monitorName mon
    joinBars = intercalate ", " . snd

initialConfigureBars bars nmons mon = do
  _ <- printMonitor mon
  configureBars mon nmons nbars bars (show $ monitorId mon, [])
  where
    nbars = length bars

indexList :: [Bar] -> [(Int, Bar)]
indexList = zip [0 ..]

main :: IO ()
main = do
  args <- getArgs
  bars <- listBars
  mapM_ (\(i, bar) -> putStrLn $ printf "%d: %s" i bar) $ indexList bars
  monitors <- listMonitors
  updatedMonitors <- mapM (initialConfigureBars bars (length monitors)) monitors
  let monitorData = encode (M.fromList updatedMonitors)
   in writeFile
        (head args ++ "/bars.json")
        (TL.unpack . TLE.decodeUtf8 $ monitorData)
