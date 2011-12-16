module Snap.Snaplet.Tasks.Types where

import           Data.ByteString
import qualified Data.Map as Map

-- | Formalized task given at command line by 
--   user. 
--   If you'd have "supercool" snaplet that would
--   register one of it's task through "/sometask"
--   route then at command line user would have to 
--   run Snap App with: 'snapapp T supercool:sometask'
--   Params for that task would be given at command line
--   like this: 'snapapp T supercool:sometask param1=v1 param2=v2'
--   This means that everything after T at command line is
--   treated as task description.
data Task = Task {
  taskNamespace :: String,
  taskName      :: String,
  taskArgs      :: Map.Map ByteString ByteString,
  taskPort      :: String
} deriving (Show)