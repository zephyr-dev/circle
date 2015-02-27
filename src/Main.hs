{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Lens ((.~), (&), (^.), _head, (^?), filtered)
import Data.Aeson (Value(..))
import Data.Aeson.Lens (key, _String, _Array)
import Network.Wreq (get, getWith, defaults, header, Response, asJSON, responseBody)
import Data.Map (Map)
import qualified Data.Vector as V
import           System.Process             (readProcessWithExitCode)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Char (isSpace)
import Control.Monad.State.Strict (StateT, runStateT)
import qualified Control.Monad.State.Strict as ST
import Data.List 
import System.Posix.Daemonize (daemonize)
import System.Directory (getCurrentDirectory, setCurrentDirectory)

{- url = "https://circleci.com/api/v1/project/zephyr-dev/gust?circle-token=a5422c509e6c049514733030174a901e8cd17b3e" -}

buildsUrl ::  String
{- branchBuildsUrl branch = "https://circleci.com/api/v1/project/zephyr-dev/gust/tree/" ++ branch ++ "?circle-token=a5422c509e6c049514733030174a901e8cd17b3e&filter=completed" -}
buildsUrl = "https://circleci.com/api/v1/project/zephyr-dev/gust?circle-token=a5422c509e6c049514733030174a901e8cd17b3e&filter=completed"


type Body = Value

opts = defaults & header "Accept" .~ ["application/json"]


type Authors = String
type Branch = String
type Build = Value

type SIO a = StateT [Build] IO a

main :: IO ()
main = do
  path <- getCurrentDirectory

  daemonize $ do
  setCurrentDirectory path
  (_, _) <- runStateT iter []
  return ()
  
    where
      iter :: SIO ()
      iter = forever $ do
        authors <- liftIO $ currentAuthors

        (r :: Response Body) <- liftIO $ asJSON =<< getWith opts buildsUrl
        let currentCompletedBuilds = V.toList $ V.filter (filterByAuthors authors) $ r ^. responseBody . _Array

        oldBuilds <- ST.get

        {- let oldBuilds = [] -}
        let newBuilds = currentCompletedBuilds \\ oldBuilds

        mapM_ (liftIO . notify) newBuilds
        ST.put $ nub $ currentCompletedBuilds ++ oldBuilds
        liftIO $ putStrLn "Checked Circle for new Builds"


        liftIO $ threadDelay $ 10 * 1000 * 1000 -- 10 seconds

          where

            filterByAuthors :: Authors -> (Build -> Bool)
            filterByAuthors authors build = build ^. key "author_name" . _String == T.pack authors



currentAuthors :: IO Authors
currentAuthors = do
  (_, authors, _) <- readProcessWithExitCode "git" ["config", "user.name"] ""
  {- putStrLn $ "current authors: " ++ authors  -}
  return $ rstrip authors



currentBranch :: IO Branch
currentBranch = do
  (_, branch, _) <- readProcessWithExitCode "git" ["symbolic-ref", "--short", "HEAD"] ""
  {- putStrLn $ "current branch: " ++ branch  -}
  return $ rstrip branch


notify :: Build -> IO ()
notify build = do
  let status = T.toUpper $ build ^. key "status" . _String
  let author = build ^. key "author_name" . _String
  let branchName = build ^. key "branch" . _String
  readProcessWithExitCode "osascript" ["-e", "display notification \"" ++ " " ++ (T.unpack branchName) ++ "\" with title \"" ++ (T.unpack status) ++ " \" "] ""

  readProcessWithExitCode "say" [T.unpack author ++ ", build " ++ (T.unpack status)] ""
  return ()
  

rstrip = reverse . dropWhile isSpace . reverse

