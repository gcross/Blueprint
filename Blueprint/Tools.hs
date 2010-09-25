-- @+leo-ver=4-thin
-- @+node:gcross.20100925004153.1313:@thin Tools.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100925004153.1314:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100925004153.1314:<< Language extensions >>
-- @nl

module Blueprint.Tools where

-- @<< Import needed modules >>
-- @+node:gcross.20100925004153.1315:<< Import needed modules >>
import Crypto.Classes
import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5

import System.FilePath

import Blueprint.Job
import Blueprint.Miscellaneous
-- @-node:gcross.20100925004153.1315:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100925004153.1317:Functions
-- @+node:gcross.20100925004153.1318:digestFile
digestFile :: FilePath → Job MD5Digest
digestFile file_to_digest =
    once (inNamespace digestFile_namespace file_to_digest)
    .
    liftIO
    $
    fmap hash (L.readFile file_to_digest)
  where
    digestFile_namespace = uuid "a901f49d-3059-4599-a86f-f953a58a96f5"
-- @-node:gcross.20100925004153.1318:digestFile
-- @-node:gcross.20100925004153.1317:Functions
-- @-others
-- @-node:gcross.20100925004153.1313:@thin Tools.hs
-- @-leo
