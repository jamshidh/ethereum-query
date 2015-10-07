{-# LANGUAGE OverloadedStrings #-}

module BlockGO
    (
     doit
    ) where

import Control.Monad
import Control.Monad.Loops
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import Data.Default
import qualified Database.LevelDB as DB
import System.Directory
import System.FilePath
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import Blockchain.Constants

import DumpLevelDB ()

import Blockchain.Format

--import Debug.Trace

ldbForEach::FilePath->(B.ByteString->B.ByteString->IO ())->IO ()
--doit dbtype h = showKeyVal (formatBlock . rlpDecode . rlpDeserialize) dbtype "blocks" (if h == "-" then Nothing else Just h)
ldbForEach dbDir' f = do
  runResourceT $ do
    db <- DB.open (dbDir' </> "blockchain") def
    i <- DB.iterOpen db def
    DB.iterFirst i
    whileM_ (DB.iterValid i) $ do
      Just key <- DB.iterKey i
      Just val <- DB.iterValue i
      liftIO $ f key val
      DB.iterNext i
      return ()

doit::String->String->IO ()
--doit dbtype h = showKeyVal (formatBlock . rlpDecode . rlpDeserialize) dbtype "blocks" (if h == "-" then Nothing else Just h)
doit _ h = do
  dbDir' <- fmap ((++ dbDir h) . (++ "/")) getHomeDirectory

  ldbForEach dbDir' $ \key val -> do
      when (B.isPrefixOf "block" key) $
        putStrLn $ "----------\n"
                     ++ show (pretty key)
                     ++ ": "
                     ++ format val
--                     ++ (formatBlock . rlpDecode . rlpDeserialize) val
