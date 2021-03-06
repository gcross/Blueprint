-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Blueprint.Tools where

-- Imports {{{
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Crypto.Classes

import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5
import Data.List.Tagged (TaggedList)
import qualified Data.List.Tagged as T
import Data.Traversable (traverse)
import Data.Typeable

import Language.Haskell.TH

import System.Directory
import System.Exit
import System.FilePath
import System.Process

import TypeLevel.NaturalNumber.Induction

import Blueprint.Identifier
import Blueprint.Job
import Blueprint.Miscellaneous
-- }}}

-- Types {{{
data FileOfType α = File -- {{{
    {   filePath :: FilePath
    ,   fileDigest :: MD5Digest
    } deriving (Eq,Show,Typeable)
-- }}}
-- }}}

-- Files Types {{{
-- ProgramFile {{{
data Program deriving Typeable
type ProgramFile = FileOfType Program
-- }}}
-- }}}

-- Exceptions {{{
data ProductionError = -- {{{
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
-- }}}
-- }}}

-- Functions {{{
declareFileType :: String → Q [Dec] -- {{{
declareFileType name = do
    return
        [DataD [] (mkName name) [] [] [mkName "Typeable"]
        ,TySynD (mkName $ name ++ "File") [] (AppT (ConT (mkName "FileOfType")) (ConT (mkName name)))
        ]
-- }}}
digestFile :: FilePath → Job MD5Digest -- {{{
digestFile filepath =
    once my_id
    .
    liftIO
    .
    fmap hash
    .
    L.readFile
    $
    filepath
  where
    my_id =
        identifierInNamespace
            (uuid "50bdbf93-8a69-497a-9493-2eb1e9f87ee0")
            ("digesting " ++ filepath)
-- }}}
digestFileIfExists :: FilePath → Job (Maybe MD5Digest) -- {{{
digestFileIfExists file_to_digest = do
    exists ← liftIO $ doesFileExist file_to_digest
    if exists
        then fmap Just (digestFile file_to_digest)
        else return Nothing
-- }}}
digestFiles :: Induction n ⇒ TaggedList n FilePath → Job (TaggedList n MD5Digest) -- {{{
digestFiles = traverse digestFile
-- }}}
runProductionCommand :: -- {{{
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
-- }}}
runProductionCommandAndDigestOutputs :: -- {{{
    Induction n ⇒
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
-- }}}
-- }}}
