{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module SalaryGraph.API where

import Control.Concurrent.STM qualified as STM
import Control.Monad.IO.Class (liftIO)
import Data.ByteString        (ByteString)
import Data.ByteString.Lazy   qualified as LBS
import Data.FileEmbed         (embedDir, embedFile)
import Data.Map               qualified as Map
import Network.HTTP.Media     ((//), (/:))
import SalaryGraph.DB         as DB
import SalaryGraph.Salary     as Salary
import Servant
import System.FilePath        (takeDirectory)

api :: Proxy API
api = Proxy

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString  where
  mimeRender _ = LBS.fromStrict

type API
      = Get '[HTML] ByteString
   :<|> "static" :> Raw
   :<|> "api" :> SalaryAPI

type SalaryAPI
      = "salaries" :> Capture "year" Year :> Get '[JSON] [Salary]
   :<|> "appointments" :> Capture "year" Year :> Get '[JSON] [Salary.Appointment]

indexHtml :: ByteString
indexHtml = $(embedFile "static/index.html")

server :: DB -> Server API
server db = pure indexHtml
    :<|> serveDirectoryEmbedded $(embedDir (takeDirectory MAINJS_FILE))
    :<|> getSalaries
    :<|> getAppointments
  where
    getSalaries year =
      liftIO $ Map.findWithDefault mempty year <$> STM.readTVarIO db.salaries
    getAppointments year =
      liftIO $ Map.findWithDefault mempty year <$> STM.readTVarIO db.appointments
