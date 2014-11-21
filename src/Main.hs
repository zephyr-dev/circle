{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.Aeson.Lens (key)
import Network.Wreq

url = "https://circleci.com/api/v1/project/zephyr-dev/gust/9138?circle-token=a5422c509e6c049514733030174a901e8cd17b3e"

type Resp = Response (Map String Value)

main :: IO ()
main = do
  r <- asJSON =<< get url :: IO Resp
  putStrLn $ r ^? responseBody . key "steps"


