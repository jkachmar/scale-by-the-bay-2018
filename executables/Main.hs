module Main
  ( main
  ) where

import           Server (runServer)

-- | Run the application server.
main :: IO ()
main = runServer
