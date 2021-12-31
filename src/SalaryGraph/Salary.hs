{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}

module SalaryGraph.Salary where

import Elm.Derive

data Salary = Salary
  { year    :: Int
  , month   :: Int
  , gross   :: Int
  , net     :: Int
  , isBonus :: Bool
  } deriving (Show, Eq)

deriveBoth defaultOptions ''Salary

data Appointment = Appointment
  { year   :: Int
  , month  :: Int
  , before :: Int
  , after  :: Int
  } deriving (Show, Eq)

deriveBoth defaultOptions ''Appointment
