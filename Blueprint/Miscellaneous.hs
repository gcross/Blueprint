-- @+leo-ver=4-thin
-- @+node:gcross.20100614121927.1659:@thin Miscellaneous.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100614121927.1660:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100614121927.1660:<< Language extensions >>
-- @nl

module Blueprint.Miscellaneous where

-- @<< Import needed modules >>
-- @+node:gcross.20100614121927.1661:<< Import needed modules >>
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Abort
import Control.Parallel.Strategies

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Digest.Pure.MD5
import Data.Typeable
import Data.UUID

import Debug.Trace

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import System.Random
-- @-node:gcross.20100614121927.1661:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100614121927.2361:Exceptions
-- @+node:gcross.20100614121927.2362:ProgramFailed
data ProgramFailed = ProgramFailed Int String deriving Typeable

instance Show ProgramFailed where
    show (ProgramFailed error_code output) =
        "Program failed with exit code " ++ show error_code ++ "\n" ++ output

instance Exception ProgramFailed
-- @-node:gcross.20100614121927.2362:ProgramFailed
-- @-node:gcross.20100614121927.2361:Exceptions
-- @+node:gcross.20100624100717.2142:Instances
-- @+node:gcross.20100624100717.2143:Typeable MD5Digest
deriving instance Typeable MD5Digest
-- @-node:gcross.20100624100717.2143:Typeable MD5Digest
-- @-node:gcross.20100624100717.2142:Instances
-- @+node:gcross.20100614121927.1662:Functions
-- @+node:gcross.20100630111926.1890:checkDigestsOfFilesIfExisting
checkDigestsOfFilesIfExisting :: [FilePath] → [MD5Digest] → IO Bool
checkDigestsOfFilesIfExisting file_paths old_digests = runAbortT $
    mapM (liftIO . doesFileExist >=> flip unless (abort False)) file_paths
    >>
    liftIO (digestFiles file_paths) >>= return . (== old_digests)
-- @-node:gcross.20100630111926.1890:checkDigestsOfFilesIfExisting
-- @+node:gcross.20100624100717.2077:digestFile
digestFile :: FilePath → IO MD5Digest
digestFile = L.readFile >=> (return .|| rwhnf) md5
-- @-node:gcross.20100624100717.2077:digestFile
-- @+node:gcross.20100624100717.2079:digestFiles
digestFiles :: [FilePath] → IO [MD5Digest]
digestFiles = mapM digestFile
-- @nonl
-- @-node:gcross.20100624100717.2079:digestFiles
-- @+node:gcross.20100614121927.2357:echo
echo x = trace (show x) x
-- @-node:gcross.20100614121927.2357:echo
-- @+node:gcross.20100614121927.1663:withTemporaryFile
withTemporaryFile :: String → (FilePath → IO a) → IO a
withTemporaryFile extension thunk = do
    directory ← getTemporaryDirectory
    filepath ← fmap ((directory </>) . (<.> extension) . show) (randomIO :: IO UUID)
    (thunk filepath) `finally` (doesFileExist filepath >>= flip when (removeFile filepath))
-- @-node:gcross.20100614121927.1663:withTemporaryFile
-- @+node:gcross.20100614121927.2360:readProcessByteString
readProcessByteString :: FilePath → [String] → String → IO S.ByteString
readProcessByteString program arguments input = do
    (Just program_input, Just program_output, _, process_id) ←
            createProcess
                (proc program arguments)
                {   std_in = CreatePipe
                ,   std_out = CreatePipe
                ,   std_err = Inherit
                }

    hPutStr program_input input
    output ← S.hGetContents program_output
    exit_code ← waitForProcess process_id

    case exit_code of
        ExitSuccess → return output
        ExitFailure error_code → throwIO $ ProgramFailed error_code (S.unpack output)
-- @-node:gcross.20100614121927.2360:readProcessByteString
-- @-node:gcross.20100614121927.1662:Functions
-- @-others
-- @-node:gcross.20100614121927.1659:@thin Miscellaneous.hs
-- @-leo
