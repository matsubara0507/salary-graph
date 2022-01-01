{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module SalaryGraph.DB where

import Control.Concurrent     (forkIO, threadDelay)
import Control.Concurrent.STM qualified as STM
import Control.Monad          (forever, when)
import Data.Aeson             (FromJSON)
import Data.Aeson             qualified as JSON
import Data.Either            (partitionEithers)
import Data.Map               (Map)
import Data.Map               qualified as Map
import GHC.Records            (HasField)
import SalaryGraph.Salary     as Salary
import System.Directory       (listDirectory)
import System.FSNotify        qualified as File
import System.FilePath        ((</>))
import System.IO              (hPutStrLn, stderr)

-- data Config = Config
--   { watch               :: Bool
--   , salariesDirPath     :: FilePath
--   , appointmentsDirPath :: FilePath
--   }

type Config r =
    ( HasField "watch" r Bool
    , HasField "salariesDirPath" r FilePath
    , HasField "appointmentsDirPath" r FilePath
    )

data DB = DB
  { salaries     :: STM.TVar (Map Year [Salary])
  , appointments :: STM.TVar (Map Year [Salary.Appointment])
  }

new :: Config r => r -> IO (Either [String] DB)
new config = do
  result1 <- readAllJSON config.salariesDirPath
  result2 <- readAllJSON config.appointmentsDirPath
  case (result1, result2) of
    (Left errs1, Left errs2) ->
        pure $ Left (errs1 ++ errs2)
    (Left errs1, _) ->
        pure $ Left errs1
    (_, Left errs2) ->
        pure $ Left errs2
    (Right files1, Right files2) -> do
        salaries <- STM.newTVarIO (toDataMap files1)
        appointments <- STM.newTVarIO (toDataMap files2)
        let db = DB {..}
        when config.watch $ do
          _ <- forkIO (watch config db)
          pure ()
        pure $ Right db

readAllJSON :: FromJSON a => FilePath -> IO (Either [String] [a])
readAllJSON dir = do
  paths <- map (dir </>) <$> listDirectory dir
  files <- mapM JSON.eitherDecodeFileStrict' paths
  pure $ case partitionEithers (files) of
    ([], fs)  -> Right $ concat fs
    (errs, _) -> Left errs

toDataMap :: HasField "year" r Year => [r] -> Map Year [r]
toDataMap = Map.fromListWith (++) . map (\f -> (f.year, [f]))

watch :: Config r => r -> DB -> IO ()
watch config db =
  File.withManager $ \mgr -> do
    _ <- File.watchDir mgr config.salariesDirPath (not . File.eventIsDirectory) $ \_ -> do
      result <- readAllJSON config.salariesDirPath
      case result of
        Left errs ->
          hPutStrLn stderr (unlines errs)
        Right files ->
          STM.atomically $ STM.writeTVar db.salaries (toDataMap files)

    _ <- File.watchDir mgr config.appointmentsDirPath (not . File.eventIsDirectory) $ \e -> do
      print e
      result <- readAllJSON config.appointmentsDirPath
      case result of
        Left errs ->
          hPutStrLn stderr (unlines errs)
        Right files ->
          STM.atomically $ STM.writeTVar db.appointments (toDataMap files)

    forever $ threadDelay 1000000
