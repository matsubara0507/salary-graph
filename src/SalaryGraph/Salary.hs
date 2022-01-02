{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module SalaryGraph.Salary where

import Data.Aeson      (FromJSON, ToJSON, ToJSONKey)
import Elm.Derive
import Web.HttpApiData (FromHttpApiData)

newtype Year = Year Int
  deriving newtype (Show, Eq, Ord, FromJSON, ToJSON, ToJSONKey, FromHttpApiData)

deriveElmDef defaultOptions ''Year

newtype Month = Month Int
  deriving newtype (Show, Eq, FromJSON, ToJSON)

deriveElmDef defaultOptions ''Month

data Salary = Salary
  { year    :: Year
  , month   :: Month
  , gross   :: Int
  , net     :: Int
  , isBonus :: Bool
  } deriving (Show, Eq)

deriveBoth defaultOptions ''Salary

data Appointment = Appointment
  { year   :: Year
  , month  :: Month
  , before :: Int
  , after  :: Int
  } deriving (Show, Eq)

deriveBoth defaultOptions ''Appointment
