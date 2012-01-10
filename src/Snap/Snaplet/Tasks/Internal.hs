module Snap.Snaplet.Tasks.Internal where

import           System( getArgs )
import           System.Exit

import           Snap.Snaplet.Tasks.Types

import           Control.Concurrent

import qualified Data.ByteString.Char8 as B
import           Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.List.Utils as U
import           Data.Hash

import           Network.Curl

-- | This function reads options from command line
--   and if it sees any options after "T" it
--   tries to compile them into task.
taskFromCommandLine :: IO (Maybe Task)
taskFromCommandLine = do
  _args <- return . dropWhile ((/=) "T") =<< getArgs
  case _args of
    [] -> return Nothing
    xs -> 
      case null $ tail xs of
        True  -> return Nothing
        False -> return $ Just $ taskForArgs $ tail xs
  where
    taskForArgs :: [String] -> Task
    taskForArgs args = Task
      (namespace heading)
      (name      heading)
      (taskargs  restargs)
      portFromArgs
      where
        heading :: [String]
        heading = U.split ":" $ head args

        portFromArgs :: String
        portFromArgs =
          case dropWhile ((/=) "-p") args of
            [] -> "8000"
            xs -> head $ tail xs

        restargs :: [(B.ByteString, B.ByteString)]
        restargs = map toKeyValue $ takeWhile ((/=) "-p") $ tail args
 
        toKeyValue :: String -> (B.ByteString, B.ByteString)
        toKeyValue s = (sp !! 0, sp !! 1)
         where
          sp = map B.pack $ U.split "=" s

        namespace :: [String] -> String
        namespace = head

        name :: [String] -> String
        name = (intercalate "/") . tail

        taskargs :: [(B.ByteString, B.ByteString)] -> Map.Map B.ByteString B.ByteString
        taskargs = Map.fromList

-- | This function lives in different thread than main app thread
--   and for given task it waits until app is ready to serve requests 
--   and then it fires request described in that task.
--   
--   For example if you had a task with namespace 'supercool' and
--   name 'task' and params id=1234 number=12345 url=google.com
--   then it'd fire PUT request for url: 
--   'localhost:[port]/supercool/task?id=1234&number=12345&url=google.com'
--   If 404 occurs it yells at user that such task does not exist.
--   For task name 'task:cool' url would start like:
--   'localhost/supercool/task/cool?id....' 
runTask :: Task -> IO ()
runTask task = waitForApp >> requestTask
  where
    waitForApp = do
      (code, _) <- curlGetString "http://localhost" 
                     [CurlPort $ read $ taskPort task]
      case code of
        CurlOK -> return ()
        _      -> threadDelay 100 >> waitForApp

    requestTask = do
      resp <- curlGetResponse taskUrl 
                        [CurlPort $ read $ taskPort task]
      case respCurlCode resp of
        CurlOK -> putStrLn   "Task ended successfully"
        _      -> 
          case respStatus resp of
            404 -> putStrLn   "No such task in application"
            _   -> putStrLn $ "Task request ended with HTTP code " ++
                                (show $ respStatus resp)

    taskUrl :: URLString
    taskUrl = "http://localhost/" ++ (taskNamespace task) ++
               "/" ++ nameForTask ++ argsToUrl

    nameForTask :: String
    nameForTask = hashString $ taskName task

    argsToUrl :: String
    argsToUrl = 
      case Map.null $ taskArgs task of
        True  -> ""
        False -> "?" ++ (intercalate "&" $ map toArg $ Map.toList $ taskArgs task)
    
    toArg :: (B.ByteString, B.ByteString) -> String
    toArg (k, v) = (B.unpack k) ++ "=" ++ (B.unpack v)

-- | This function takes a string and returns a hash for it
hashString :: String -> String
hashString s = show $ asWord64 $ foldl combine (hashInt $ length s) $ map hash s