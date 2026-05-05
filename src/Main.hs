module Main (main) where

import Integration
import Runner

main :: IO ()
main = do
  Runner{options, runHakyll} <- getRunner
  integrateExec options runHakyll
