-- @+leo-ver=4-thin
-- @+node:gcross.20100925004153.1313:@thin Tools.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100925004153.1314:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100925004153.1314:<< Language extensions >>
-- @nl

module Blueprint.Tools where

-- @<< Import needed modules >>
-- @+node:gcross.20100925004153.1315:<< Import needed modules >>
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Crypto.Classes

import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5
import Data.List.Tagged (TaggedList)
import qualified Data.List.Tagged as T
import Data.NaturalNumber
import Data.Traversable (traverse)
import Data.Typeable

import Language.Haskell.TH

import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Blueprint.Job
import Blueprint.Miscellaneous
-- @-node:gcross.20100925004153.1315:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100929125042.1464:Types
-- @+node:gcross.20101010201506.1500:FileOfType
data FileOfType α = File
    {   filePath :: FilePath
    ,   fileDigest :: MD5Digest
    } deriving Typeable
-- @nonl
-- @-node:gcross.20101010201506.1500:FileOfType
-- @-node:gcross.20100929125042.1464:Types
-- @+node:gcross.20101010201506.1503:File Types
-- @+node:gcross.20101010201506.1504:Program
data Program deriving Typeable
type ProgramFile = FileOfType Program
-- @nonl
-- @-node:gcross.20101010201506.1504:Program
-- @-node:gcross.20101010201506.1503:File Types
-- @+node:gcross.20100927222551.1483:Exceptions
-- @+node:gcross.20100927222551.1484:ProductionError
data ProductionError =
    ProductionCommandFailed String String
  | FailedToProduceMandatoryOutputs [FilePath]
  deriving (Typeable)

instance Show ProductionError where
    show (ProductionCommandFailed production_command error_message) =
        "Error executing command '" ++ production_command ++ "':\n"
        ++
        error_message
    show (FailedToProduceMandatoryOutputs non_existing_mandatory_outputs) =
        "The following output files were expected but not produced: " ++ show non_existing_mandatory_outputs

instance Exception ProductionError
-- @-node:gcross.20100927222551.1484:ProductionError
-- @-node:gcross.20100927222551.1483:Exceptions
-- @+node:gcross.20100925004153.1317:Functions
-- @+node:gcross.20101010201506.1509:declareFileType
declareFileType :: String → Q [Dec]
declareFileType name = do
    return
        [DataD [] (mkName name) [] [] [mkName "Typeable"]
        ,TySynD (mkName $ name ++ "File") [] (AppT (ConT (mkName "FileOfType")) (ConT (mkName name)))
        ]
-- @-node:gcross.20101010201506.1509:declareFileType
-- @+node:gcross.20100925004153.1318:digestFile
digestFile :: FilePath → Job MD5Digest
digestFile filepath =
    once (inMyNamespace filepath)
    .
    liftIO
    .
    fmap hash
    .
    L.readFile
    $
    filepath
  where
    inMyNamespace = inNamespace (uuid "50bdbf93-8a69-497a-9493-2eb1e9f87ee0")
-- @-node:gcross.20100925004153.1318:digestFile
-- @+node:gcross.20100927222551.1459:digestFileIfExists
digestFileIfExists :: FilePath → Job (Maybe MD5Digest)
digestFileIfExists file_to_digest = do
    exists ← liftIO $ doesFileExist file_to_digest
    if exists
        then fmap Just (digestFile file_to_digest)
        else return Nothing
-- @-node:gcross.20100927222551.1459:digestFileIfExists
-- @+node:gcross.20100927222551.1486:digestFiles
digestFiles :: NaturalNumber n ⇒ TaggedList n FilePath → Job (TaggedList n MD5Digest)
digestFiles = traverse digestFile
-- @-node:gcross.20100927222551.1486:digestFiles
-- @+node:gcross.20101009103525.1737:runProductionCommand
runProductionCommand ::
    MonadIO m ⇒
    String →
    [String] →
    String →
    m ()
runProductionCommand command arguments input = liftIO $ do
    (exit_code,_,output) ←
        readProcessWithExitCode
            command
            arguments
            input
    when (exit_code /= ExitSuccess) . throwIO $
        ProductionCommandFailed (unwords (command:arguments)) output
-- @nonl
-- @-node:gcross.20101009103525.1737:runProductionCommand
-- @+node:gcross.20100927222551.1472:runProductionCommandAndDigestOutputs
runProductionCommandAndDigestOutputs ::
    NaturalNumber n ⇒
    TaggedList n FilePath →
    String →
    [String] →
    Job (TaggedList n MD5Digest)
runProductionCommandAndDigestOutputs
    mandatory_product_filepaths
    command
    arguments
  =
  do
    liftIO $ do
        T.mapM_ (createDirectoryIfMissing True . takeDirectory) $ mandatory_product_filepaths
        runProductionCommand command arguments ""
        mandatory_products_not_existing ←
            filterM (fmap not . doesFileExist) (T.toList mandatory_product_filepaths)
        when (not . null $ mandatory_products_not_existing) . throwIO $
            FailedToProduceMandatoryOutputs mandatory_products_not_existing
    digestFiles mandatory_product_filepaths
-- @nonl
-- @-node:gcross.20100927222551.1472:runProductionCommandAndDigestOutputs
-- @-node:gcross.20100925004153.1317:Functions
-- @-others
-- @-node:gcross.20100925004153.1313:@thin Tools.hs
-- @-leo
