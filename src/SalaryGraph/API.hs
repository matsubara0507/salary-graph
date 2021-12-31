{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module SalaryGraph.API where

import Data.ByteString      (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.FileEmbed       (embedFile)
import Data.Map             (Map)
import Network.HTTP.Media   ((//), (/:))
import SalaryGraph.Salary   as Salary
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
      = "salaries" :> Get '[JSON] (Map Int [Salary])
   :<|> "salaries" :> QueryParam' '[Required] "year" Integer :> Get '[JSON] [Salary]
   :<|> "appointments" :> Get '[JSON] (Map Int [Salary.Appointment])
   :<|> "appointments" :> QueryParam' '[Required] "year" Integer :> Get '[JSON] [Salary.Appointment]

indexHtml :: ByteString
indexHtml = $(embedFile "static/index.html")

server :: Monad m => ServerT API m
server = pure indexHtml
    :<|> getAllSalaries
    :<|> getSalaries
    :<|> getAllAppointments
    :<|> getAppointments
  where
    getAllSalaries = undefined
    getSalaries = undefined
    getAllAppointments = undefined
    getAppointments = undefined
