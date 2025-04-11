{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (IOException, try)
import Data.Aeson (decode, encode)
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import Data.Char (isDigit)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Process (readCreateProcess, shell)
import Text.Read (readEither)

data Monitor =
  Monitor
    { monitorName :: String
    , monitorId :: Int
    }
  deriving (Show)

type Config = Map String [Bar]

type Bar = String

parseMonitor :: String -> Monitor
parseMonitor monStr = Monitor parseMonitorName parseMonitorId
  where
    parseMonitorId = read . takeWhile (/= ':') . dropWhile (== ' ') $ monStr
    parseMonitorName = reverse . takeWhile (/= ' ') . reverse $ monStr

listBars :: IO [Bar]
listBars = filter (isSuffixOf ".hs") <$> getDirectoryContents "xmobar"

listMonitors :: IO [Monitor]
listMonitors = do
  output <- readCreateProcess (shell "xrandr --listmonitors") ""
  return . map parseMonitor . drop 1 . lines $ output

printMonitor :: Monitor -> IO ()
printMonitor mon = putStrLn $ "Processing monitor: " ++ monitorName mon

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy delimiterChar inputString = foldr f [""] inputString
  where
    f :: Char -> [String] -> [String]
    f _ [] = error "huh"
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
      show b ++ ", only (0-" ++ show (i - 1) ++ ")"

type MonitorState = (String, [String])

updateMonitor :: [Bar] -> [Int] -> MonitorState -> IO MonitorState
updateMonitor bars barIndexes (monId, barAcc) =
  return (monId, barAcc `L.union` map (bars !!) barIndexes)

prompt :: Int -> Int -> String
prompt nbars nmons
  | nmons == 0 = "Add new bar or exit?" ++ barsRange
  | nmons == 1 = "Add new bar or continue to last monitor?" ++ barsRange
  | otherwise = "Add new bar or continue to next monitor?" ++ barsRange
  where
    barsRange = " (0-" ++ show (nbars - 1) ++ ", c or d): "

data Action
  = Continue
  | DeleteBars
  | SelectBars [Int]

selectAction :: Int -> Either IOException Bar -> Either String Action
selectAction nbars (Right bs)
  | bs == "" = Left "empty input"
  | bs `isPrefixOf` "continue" = Right Continue
  | bs `isPrefixOf` "delete" = Right DeleteBars
  | otherwise = fmap SelectBars . mapM (readBarIndex nbars) . splitBy ',' $ bs
selectAction _ (Left _) = Right Continue

printSelectedBars :: MonitorState -> String -> IO ()
printSelectedBars (_, bars) monName
  | null bars = putStrLn $ "No bars monitor " ++ monName
  | otherwise =
    putStrLn $
    "Current bars on monitor " ++ monName ++ ": " ++ intercalate ", " bars

configureBars ::
     Int -> Monitor -> Int -> [Bar] -> MonitorState -> IO MonitorState
configureBars nmons mon nbars bars state = do
  printSelectedBars state $ monitorName mon
  putStrLn . prompt nbars $ nmons - monitorId mon - 1
  bs <- try getLine
  case selectAction nbars bs of
    Left err -> putStrLn ("error: " ++ err) >> configureBars' state
    Right Continue -> return state
    Right DeleteBars -> configureBars' (fst state, [])
    Right (SelectBars bs') -> updateMonitor bars bs' state >>= configureBars'
  where
    configureBars' = configureBars nmons mon nbars bars

tryReadConfigFile :: FilePath -> IO (Either IOException ByteString)
tryReadConfigFile = try . BL.readFile

decodeConfig :: FilePath -> IO Config
decodeConfig filename = do
  content <- tryReadConfigFile filename
  case content of
    Right content' -> return . fromMaybe M.empty . decode $ content'
    Left _ -> return M.empty

initialConfigureBars :: Int -> Monitor -> [Bar] -> [Bar] -> IO MonitorState
initialConfigureBars nmons mon bars selectedBars =
  printMonitor mon >> configureBars nmons mon nbars bars state
  where
    state = (show $ monitorId mon, selectedBars)
    nbars = length bars

indexList :: [Bar] -> [(Int, Bar)]
indexList = zip [0 ..]

validateConfigDir :: [String] -> IO (Maybe String)
validateConfigDir (d:_) =
  doesDirectoryExist d >>= bool (pure Nothing) (pure . Just $ d ++ "/bars.json")
validateConfigDir _ = pure Nothing

printBar :: (Int, String) -> IO ()
printBar (i, bar) = putStrLn $ show i ++ ": " ++ bar

fetchMonConfig :: Monitor -> Map String [Bar] -> [Bar]
fetchMonConfig mon = fromMaybe [] . M.lookup (show $ monitorId mon)

barSelectorForMonitor :: Config -> [Bar] -> Int -> Monitor -> IO MonitorState
barSelectorForMonitor config bars nmons mon =
  initialConfigureBars nmons mon bars $ fetchMonConfig mon config

printBars :: [Bar] -> IO ()
printBars bars
  | null bars = putStrLn "no bars found" >> exitFailure
  | otherwise = putStrLn "Available bars:" >> mapM_ printBar (indexList bars)

startBarSelector :: String -> IO ()
startBarSelector configFile = do
  bars <- listBars
  printBars bars
  mons <- listMonitors
  config <- decodeConfig configFile
  updatedMonitors <- mapM (barSelectorForMonitor config bars $ length mons) mons
  BL.writeFile configFile . encode . M.fromList $ updatedMonitors

main :: IO ()
main = do
  args <- getArgs
  configDir <- validateConfigDir args
  case configDir of
    Nothing ->
      putStrLn
        "please provide xmonad configuration directory and make sure it is exists"
    Just configFile -> startBarSelector configFile
