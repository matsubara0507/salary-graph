module Main where

import Network.Wai.Handler.Warp qualified as Warp
import SalaryGraph.API          qualified as SalaryGraph
import Servant                  (serve)

main :: IO ()
main = do
  putStrLn "Listening on port 8080"
  Warp.run 8080 $ serve SalaryGraph.api SalaryGraph.server
