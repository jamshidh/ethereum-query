{-# LANGUAGE OverloadedStrings #-}

module RawMP
    (
     doit
    ) where

import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Functor
import Data.List
import qualified Database.LevelDB as DB
import System.Directory
import System.Environment
import System.FilePath
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (</>))

import qualified Data.NibbleString as N
import Blockchain.Data.RLP

import Blockchain.Constants
import Blockchain.Context
import Blockchain.DB.StateDB
import Blockchain.DBM
import Blockchain.ExtDBs
import Blockchain.Format
import qualified Blockchain.Database.MerklePatricia as MP
import qualified Blockchain.Database.MerklePatricia.Internal as MP

formatKV::(N.NibbleString, RLPObject)->Doc
formatKV (key, val) =
    pretty key <> text ": " <> pretty (rlpDeserialize $ rlpDecode val)

--showVals::SHAPtr->ContextM ()
showVals sdb sr = do
  kvs <- MP.unsafeGetKeyVals MP.MPDB{MP.ldb=sdb, MP.stateRoot=sr} ""
  liftIO $ putStrLn $ show $ length kvs
  --liftIO $ putStrLn $ displayS (renderPretty 1.0 200 $ vsep $ formatKV <$> kvs) ""
  liftIO $ putStrLn $ displayS (renderPretty 1.0 200 $ vsep $ formatKV <$> kvs) "" 

doit::String->SHAPtr->IO()
doit filename sr = do
  DB.runResourceT $ do
--    dbs <- openDBs theType
    homeDir <- liftIO getHomeDirectory                     
    sdb <- DB.open filename
           DB.defaultOptions{DB.cacheSize=1024}


    showVals sdb sr
    return ()

