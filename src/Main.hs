{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens ((.~), (&), (^.), _head, (^?), filtered)
import Data.Aeson (Value(..))
import Data.Aeson.Lens (key, _String, _Array)
import Network.Wreq (get, getWith, defaults, header, Response, asJSON, responseBody)
import Data.Map (Map)
import qualified Data.Vector as V

url = "https://circleci.com/api/v1/project/zephyr-dev/gust?circle-token=a5422c509e6c049514733030174a901e8cd17b3e"

{- type Build = Value -}
{- type Body = [Build] -}
type Body = Value

opts = defaults & header "Accept" .~ ["application/json"]

main :: IO ()
main = do
  (r :: Response Body) <- asJSON =<< getWith opts url
  let builds = r ^. responseBody . _Array -- . filtered (\b -> (b ^? key "status" . _String) == "running") 
  print $ V.length $ V.filter (\b -> b ^. key "status" . _String == "running") builds  -- == "running"

  {- putStrLn $ show $ builds -}

  {- where -}
    {- filterRunning :: [Build] -> [Build] -}
    {- filterRunning = filter running -}

    {- running :: Build -> Bool -}
    {- running b = b ^. key "status" . _String == "running" -}

