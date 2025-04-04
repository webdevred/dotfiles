{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import Data.Char (isDigit)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map (Map)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
import System.Process (readCreateProcess, shell)
import Text.Read (readEither)

import Data.Maybe (fromMaybe)

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
    f currentChar [] = error "huh"
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
prompt nbars _ =
  "Add new bar or continue to next monitor? (0-" ++ nbars ++ " or c)"

configureBars ::
     Monitor -> Int -> Int -> [Bar] -> MonitorState -> IO MonitorState
configureBars mon nmons nbars bars state = do
  when
    (snd state /= [])
    (putStrLn $ "Current bars on monitor " ++ monName ++ ": " ++ joinBars state)
  _ <- putStrLn $ prompt (show $ nbars - 1) $ nmons - monitorId mon
  bs <- getLine
  if not $ isPrefixOf bs "continue"
    then do
      updateMonitor bars bs state >>= configureBars mon nmons nbars bars
    else return state
  where
    monName = monitorName mon
    joinBars = intercalate ", " . snd

tryReadConfigFile :: FilePath -> IO (Either IOException ByteString)
tryReadConfigFile = try . BL.readFile

decodeConfig :: FilePath -> IO Config
decodeConfig filename = do
  content <- tryReadConfigFile filename
  case content of
    Right content -> return . fromMaybe M.empty . decode $ content
    Left _ -> return M.empty

initialConfigureBars :: [Bar] -> Int -> [Bar] -> Monitor -> IO MonitorState
initialConfigureBars selectedBars nmons bars mon = do
  _ <- printMonitor mon
  configureBars mon nmons nbars bars (show $ monitorId mon, selectedBars)
  where
    nbars = length bars

indexList :: [Bar] -> [(Int, Bar)]
indexList = zip [0 ..]

if' :: Bool -> p -> p -> p
if' True a _ = a
if' False _ b = b

validateConfigDir :: [String] -> IO (Maybe String)
validateConfigDir args =
  case L.uncons args of
    Just (dir, _) -> do
      exists <- doesDirectoryExist dir
      return $ if' exists (Just $ dir ++ "/bars.json") Nothing
    Nothing -> return Nothing

printBar :: (Int, String) -> IO ()
printBar (i, bar) = putStrLn $ show i ++ ": " ++ bar

fetchMonConfig :: Monitor -> Map String [Bar] -> [Bar]
fetchMonConfig mon = fromMaybe [] . M.lookup (show $ monitorId mon)

barSelectorForMonitor :: Config -> [Bar] -> Int -> Monitor -> IO MonitorState
barSelectorForMonitor config bars nmons mon =
  initialConfigureBars (fetchMonConfig mon config) nmons bars mon

startBarSelector :: String -> IO ()
startBarSelector configFile = do
  bars <- listBars
  mapM_ printBar $ indexList bars
  mons <- listMonitors
  config <- decodeConfig configFile
  updatedMonitors <- mapM (barSelectorForMonitor config bars $ length mons) mons
  BL.writeFile configFile $ encode (M.fromList updatedMonitors)

main :: IO ()
main = do
  args <- getArgs
  configDir <- validateConfigDir args
  case configDir of
    Nothing ->
      putStrLn
        "please provide xmonad configuration directory and make sure it is exists"
    Just configFile -> startBarSelector configFile
