{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}

module Main where

import Paths_salary_graph       (version)

import Control.Monad            (when)
import Data.Version             qualified as Version
import Network.Wai.Handler.Warp qualified as Warp
import SalaryGraph.API          qualified as SalaryGraph
import SalaryGraph.DB           qualified as DB
import Servant                  (serve)
import System.Console.GetOpt
import System.Environment       (getArgs)
import Text.Read                (readMaybe)

main :: IO ()
main = do
  opts <- compilerOpts usage =<< getArgs
  if
    | opts.help    -> putStrLn usage
    | opts.version -> putStrLn $ Version.showVersion version
    | otherwise    -> runServer opts
  where
    usage = usageInfo "Usage: salary-graph [OPTION...]" options

runServer :: Options -> IO ()
runServer opts = do
  db <- DB.new opts
  case db of
    Left errs ->
      ioError (userError $ unlines errs)
    Right db' -> do
      when opts.verbose $
        putStrLn ("Listening on port " ++ show opts.port)
      Warp.run opts.port $
        serve SalaryGraph.api (SalaryGraph.server db')

data Options = Options
  { help                :: Bool
  , version             :: Bool
  , verbose             :: Bool
  , port                :: Int
  , watch               :: Bool
  , salariesDirPath     :: FilePath
  , appointmentsDirPath :: FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { help    = False
  , version = False
  , verbose = False
  , port    = 8080
  , watch   = False
  , salariesDirPath = "./data/salaries"
  , appointmentsDirPath = "./data/appointments"
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["help"]
      (NoArg (\opts -> opts { help = True }))
      "Show this help text"
  , Option ['V'] ["version"]
      (NoArg (\opts -> opts { version = True }))
      "Show version"
  , Option ['v'] ["verbose"]
      (NoArg (\opts -> opts { verbose = True }))
      "Enable verbose mode"
  , Option ['p'] ["port"]
      (ReqArg (\port opts -> maybe opts (\p -> opts { port = p }) (readMaybe port)) "PORT")
      "Port for server (default is 8080)"
  , Option [] ["watch"]
      (NoArg (\opts -> opts { verbose = True }))
      "Enable watch data"
  , Option [] ["salaries"]
      (ReqArg (\port opts -> maybe opts (\p -> opts { port = p }) (readMaybe port)) "PATH")
      "Path to directory of salary data (default is ./data/salaries)"
  , Option [] ["appointments"]
      (ReqArg (\port opts -> maybe opts (\p -> opts { port = p }) (readMaybe port)) "PATH")
      "Path to directory of appointment data (default is ./data/appointments)"
  ]

compilerOpts :: String -> [String] -> IO Options
compilerOpts usage argv =
  case getOpt Permute options argv of
    (o, _, []  ) -> pure $ foldl (flip id) defaultOptions o
    (_, _, errs) -> ioError $ userError (concat errs ++ usage)
