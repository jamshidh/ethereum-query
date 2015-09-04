{-# LANGUAGE OverloadedStrings #-}

module Raw (
  doit
  ) where

import Control.Monad
import Control.Monad.Loops
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Default
import qualified Database.LevelDB as DB
import System.Directory
--import System.Environment
import System.FilePath
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.Constants
import Blockchain.Data.RLP

import Blockchain.Data.BlockDB

import DumpLevelDB

import Blockchain.Format

--import Debug.Trace

ldbForEach::FilePath->(B.ByteString->B.ByteString->IO ())->IO ()
ldbForEach dbDir f = do
  runResourceT $ do
    db <- DB.open dbDir def
    i <- DB.iterOpen db def
    DB.iterFirst i
    whileM_ (DB.iterValid i) $ do
      Just key <- DB.iterKey i
      Just val <- DB.iterValue i
      liftIO $ f key val
      DB.iterNext i
      return ()

doit::String->IO ()
doit filename = do
  ldbForEach filename $ \key val ->
    putStrLn $ "----------\n"
      ++ show (pretty key)
      ++ ": "
      ++ format val
