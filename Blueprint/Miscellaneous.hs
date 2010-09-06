-- @+leo-ver=4-thin
-- @+node:gcross.20100614121927.1659:@thin Miscellaneous.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100614121927.1660:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100614121927.1660:<< Language extensions >>
-- @nl

module Blueprint.Miscellaneous where

-- @<< Import needed modules >>
-- @+node:gcross.20100614121927.1661:<< Import needed modules >>
import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Abort
import Control.Parallel.Strategies

import Data.Binary
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.DeriveTH
import Data.Digest.Pure.MD5
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Data.UUID
import Data.Version

import Debug.Trace

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import System.Random

import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Regex.Base
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
-- @+node:gcross.20100831211145.2129:Classes
-- @+node:gcross.20100831211145.2130:Cofunctor
class Cofunctor f where
    cofmap :: (b → a) → f a → f b
-- @-node:gcross.20100831211145.2130:Cofunctor
-- @-node:gcross.20100831211145.2129:Classes
-- @+node:gcross.20100624100717.2142:Instances
-- @+node:gcross.20100830091258.2046:Binary Version
$(derive makeBinary ''Version)
-- @-node:gcross.20100830091258.2046:Binary Version
-- @+node:gcross.20100624100717.2143:Typeable MD5Digest
deriving instance Typeable MD5Digest
-- @-node:gcross.20100624100717.2143:Typeable MD5Digest
-- @-node:gcross.20100624100717.2142:Instances
-- @+node:gcross.20100614121927.1662:Functions
-- @+node:gcross.20100905161144.1954:(≠)
(≠) :: Eq a => a → a → Bool
(≠) = (/=)
-- @-node:gcross.20100905161144.1954:(≠)
-- @+node:gcross.20100830091258.2038:addExe
addExe :: String → String
addExe = id
-- @-node:gcross.20100830091258.2038:addExe
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
-- @+node:gcross.20100903200211.2254:doubleton
doubleton x y = [x,y]
-- @-node:gcross.20100903200211.2254:doubleton
-- @+node:gcross.20100614121927.2357:echo
echo x = trace (show x) x
-- @-node:gcross.20100614121927.2357:echo
-- @+node:gcross.20100830091258.2026:extractVersion
extractVersion ::
    RegexLike regex String =>
    regex →
    String →
    Maybe Version
extractVersion regex string =
    case mrSubList (match regex string) of
        v:[] → tryReadVersion v
        _ → Nothing
-- @-node:gcross.20100830091258.2026:extractVersion
-- @+node:gcross.20100902134026.2126:gather
gather :: (Ord a, Eq a) => [(a,b)] → [(a,[b])]
gather =
    map (fst . head &&& map snd)
    .
    groupBy ((==) `on` fst)
    .
    sortBy (compare `on` fst)
-- @-node:gcross.20100902134026.2126:gather
-- @+node:gcross.20100903200211.2251:intersectAndUnion
intersectAndUnion :: Ord k => (a → b → b) → Map k a → Map k b → Map k b
intersectAndUnion combine x y = Map.union (Map.intersectionWith combine x y) y
-- @-node:gcross.20100903200211.2251:intersectAndUnion
-- @+node:gcross.20100830091258.2041:invertMap
invertMap :: Ord b => Map a b → Map b [a]
invertMap =
    Map.fromList
    .
    map ((fst . head) &&& map snd)
    .
    groupBy ((==) `on` fst)
    .
    sortBy (compare `on` fst)
    .
    map (\(a,b) → (b,a))
    .
    Map.toList
-- @-node:gcross.20100830091258.2041:invertMap
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
-- @+node:gcross.20100709210816.2203:readVersion
readVersion :: String -> Version
readVersion s =
    fromMaybe
        (error $ "Unable to parse version string '" ++ s ++ "'")
        (tryReadVersion s)
-- @-node:gcross.20100709210816.2203:readVersion
-- @+node:gcross.20100830091258.2024:tryReadVersion
tryReadVersion :: String → Maybe Version
tryReadVersion string =
    case readP_to_S parseVersion string of 
        [] → Nothing 
        parses → (Just . fst . last) parses

-- @-node:gcross.20100830091258.2024:tryReadVersion
-- @+node:gcross.20100614121927.1663:withTemporaryFile
withTemporaryFile :: String → (FilePath → IO a) → IO a
withTemporaryFile extension thunk = do
    directory ← getTemporaryDirectory
    filepath ← fmap ((directory </>) . (<.> extension) . show) (randomIO :: IO UUID)
    (thunk filepath) `finally` (doesFileExist filepath >>= flip when (removeFile filepath))
-- @-node:gcross.20100614121927.1663:withTemporaryFile
-- @-node:gcross.20100614121927.1662:Functions
-- @-others
-- @-node:gcross.20100614121927.1659:@thin Miscellaneous.hs
-- @-leo
