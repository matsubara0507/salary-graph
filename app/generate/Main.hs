{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import Data.Maybe         (fromMaybe, listToMaybe)
import SalaryGraph.API    as Salary
import SalaryGraph.Salary as Salary
import Servant            ((:>))
import Servant.Elm
import System.Environment (getArgs)

main :: IO ()
main = do
  dirPath <- fromMaybe "elm-src" . listToMaybe <$> getArgs
  generateElmModuleWith
    defElmOptions
    ["Generated", "SalaryAPI"]
    defElmImports
    dirPath
    [ DefineElm (Proxy @Salary)
    , DefineElm (Proxy @Salary.Year)
    , DefineElm (Proxy @Salary.Month)
    , DefineElm (Proxy @Salary.Appointment)
    ]
    (Proxy @("api" :> SalaryAPI))

