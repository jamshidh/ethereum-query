
module Hash
    (
     doit
    ) where

import Control.Monad.Trans.Resource
import Data.Default
import qualified Database.LevelDB as DB
import System.FilePath
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.Data.RLP

import Blockchain.Data.Code

import DumpLevelDB

import Blockchain.Format

--import Debug.Trace

formatCode::Code->String
formatCode = show

doit::String->String->IO ()
doit dbtype h = showKeyVal format dbtype "state" (if h == "-" then Nothing else Just h)






