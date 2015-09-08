
module RLP
    (
     doit
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Default
import qualified Database.LevelDB as DB
import System.Directory
import System.Environment
import System.FilePath
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.Data.RLP
import Blockchain.Format

import Util

--import Debug.Trace

doit::String->IO ()
doit filename = do
  ldbForEach filename $ \key val -> do
    putStrLn $ format key ++ ":" ++ tab ("\n" ++ formatRLPObject (rlpDeserialize val))
    putStrLn "--------------------"





