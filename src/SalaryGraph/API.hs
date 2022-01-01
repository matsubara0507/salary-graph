{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module SalaryGraph.API where

import Control.Concurrent.STM qualified as STM
import Control.Monad.IO.Class (liftIO)
import Data.ByteString        (ByteString)
import Data.ByteString.Lazy   qualified as LBS
import Data.FileEmbed         (embedFile)
import Data.Map               (Map)
import Data.Map               qualified as Map
import Network.HTTP.Media     ((//), (/:))
import SalaryGraph.DB         as DB
import SalaryGraph.Salary     as Salary
import Servant

api :: Proxy API
api = Proxy

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString  where
  mimeRender _ = LBS.fromStrict

type API
      = Get '[HTML] ByteString
   :<|> "api" :> SalaryAPI

type SalaryAPI
      = "salaries" :> Get '[JSON] (Map Year [Salary])
   :<|> "salaries" :> Capture "year" Year :> Get '[JSON] [Salary]
   :<|> "appointments" :> Get '[JSON] (Map Year [Salary.Appointment])
   :<|> "appointments" :> Capture "year" Year :> Get '[JSON] [Salary.Appointment]

indexHtml :: ByteString
indexHtml = $(embedFile "static/index.html")

server :: DB -> Server API
server db = pure indexHtml
    :<|> getAllSalaries
    :<|> getSalaries
    :<|> getAllAppointments
    :<|> getAppointments
  where
    getAllSalaries = liftIO $ STM.readTVarIO db.salaries
    getSalaries year = liftIO $ Map.findWithDefault mempty year <$> STM.readTVarIO db.salaries
    getAllAppointments = liftIO $ STM.readTVarIO db.appointments
    getAppointments year = liftIO $ Map.findWithDefault mempty year <$> STM.readTVarIO db.appointments
