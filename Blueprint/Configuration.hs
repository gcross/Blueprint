-- @+leo-ver=4-thin
-- @+node:gcross.20091123215917.1369:@thin Configuration.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091126122246.1386:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- @-node:gcross.20091126122246.1386:<< Language extensions >>
-- @nl

module Blueprint.Configuration where

-- @<< Import needed modules >>
-- @+node:gcross.20091126122246.1381:<< Import needed modules >>
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.RWS (RWS(..),runRWS)
import Control.Monad.Trans
import Control.Monad.Writer

import Data.ConfigFile
import Data.Dynamic
import Data.Either.Unwrap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable

import StringTable.Atom
import StringTable.AtomMap (AtomMap)
import qualified StringTable.AtomMap as AtomMap

import System.IO
import System.IO.Error
import System.IO.Unsafe

import Blueprint.Error
import Blueprint.Miscellaneous
import Blueprint.Options
-- @-node:gcross.20091126122246.1381:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091123215917.1370:Classes
-- @+node:gcross.20091123215917.1371:ConfigurationData
class ConfigurationData a where
    readConfig :: ConfigurationDataReader a
    writeConfig :: a -> ConfigurationDataWriter ()
-- @-node:gcross.20091123215917.1371:ConfigurationData
-- @+node:gcross.20091128000856.1408:AutomaticallyConfigurable
class AutomaticallyConfigurable a where
    automaticallyConfigure :: ParsedOptions -> Either ErrorMessage a
-- @-node:gcross.20091128000856.1408:AutomaticallyConfigurable
-- @-node:gcross.20091123215917.1370:Classes
-- @+node:gcross.20091126122246.1387:Instances
-- @+node:gcross.20091126122246.1388:Monoid (ConfigParser -> ConfigParser)
instance Monoid (Automorphism a) where
    mempty = A id
    mappend (A g) (A f) = A (f . g)
-- @-node:gcross.20091126122246.1388:Monoid (ConfigParser -> ConfigParser)
-- @+node:gcross.20091127142612.1407:Applicative (ConfigurationDataWriter)
instance Applicative ConfigurationDataWriter where
    pure = return
    x <*> y = WriterT . Reader $
        \environment ->
            let (function,w1) = (runReader . runWriterT) x $ environment
                (argument,w2) = (runReader . runWriterT) y $ environment
            in (function argument,w1 `mappend` w2)
-- @-node:gcross.20091127142612.1407:Applicative (ConfigurationDataWriter)
-- @-node:gcross.20091126122246.1387:Instances
-- @+node:gcross.20091123215917.1372:Types
-- @+node:gcross.20091123215917.1373:ConfigurationDataReader/Writer
type ConfigurationDataReader = ErrorT CPError (Reader (ConfigParser,String))
type ConfigurationDataWriter = WriterT (Automorphism ConfigParser) (Reader String)
-- @-node:gcross.20091123215917.1373:ConfigurationDataReader/Writer
-- @+node:gcross.20091129000542.1488:Environment
data Environment = Environment
    {   environmentConfigParser :: ConfigParser
    ,   environmentOptions :: ParsedOptions
    }
-- @-node:gcross.20091129000542.1488:Environment
-- @+node:gcross.20091128000856.1411:Configurer
type Configurer = ErrorT ErrorMessage (RWS Environment (Automorphism ConfigParser) ())

-- @-node:gcross.20091128000856.1411:Configurer
-- @+node:gcross.20091126122246.1389:Automorphism
newtype Automorphism a = A { unA :: a -> a }
-- @-node:gcross.20091126122246.1389:Automorphism
-- @+node:gcross.20091129000542.1573:ConfigurationKey
newtype ConfigurationKey = ConfigurationKey { unwrapConfigurationKey :: String }
-- @-node:gcross.20091129000542.1573:ConfigurationKey
-- @-node:gcross.20091123215917.1372:Types
-- @+node:gcross.20091126122246.1379:Functions
-- @+node:gcross.20091201134050.1974:signalRefreshNeeded
signalRefreshNeeded :: (MonadError CPError m) => m a
signalRefreshNeeded = throwError (NoOption undefined,undefined)
-- @-node:gcross.20091201134050.1974:signalRefreshNeeded
-- @+node:gcross.20091129000542.1574:makeConfigurationKey
makeConfigurationKey = ConfigurationKey
-- @-node:gcross.20091129000542.1574:makeConfigurationKey
-- @+node:gcross.20091126122246.1380:getConfig
getConfig :: Get_C a => ConfigurationKey -> ConfigurationDataReader a
getConfig (ConfigurationKey key) = do
    (cp,section) <- lift $ ask
    get cp section key
-- @-node:gcross.20091126122246.1380:getConfig
-- @+node:gcross.20091126122246.1385:setConfig
setConfig :: ConfigurationKey -> String -> ConfigurationDataWriter ()
setConfig (ConfigurationKey key) value = do
    section :: String <- lift ask
    tell . A $
         \cp ->
            case set cp section key value of
                Right cp -> cp
                Left (NoSection _,_) ->
                    case (add_section cp section >>= \new_cp -> set new_cp section key value) of
                        Right cp -> cp
                        Left e -> raiseCPError section e
                Left e -> raiseCPError section e
  where
    raiseCPError :: String -> CPError -> ConfigParser
    raiseCPError section e =
        error $ "Error adding section " ++ show section
             ++ " key " ++ show key
             ++ " value " ++ show value
             ++ ":" ++ show e
-- @-node:gcross.20091126122246.1385:setConfig
-- @+node:gcross.20091127142612.1423:applyWriterToConfig
applyWriterToConfig :: String -> ConfigurationDataWriter () -> (ConfigParser -> ConfigParser)
applyWriterToConfig section_name = unA . snd . ($ section_name) . runReader . runWriterT
-- @-node:gcross.20091127142612.1423:applyWriterToConfig
-- @+node:gcross.20091127142612.1424:applyReaderToConfig
applyReaderToConfig :: ConfigParser -> String -> ConfigurationDataReader a -> Either CPError a
applyReaderToConfig config_parser section_name = ($ (config_parser,section_name)) . runReader . runErrorT
-- @-node:gcross.20091127142612.1424:applyReaderToConfig
-- @+node:gcross.20091128000856.1412:runConfigurer
runConfigurer :: FilePath -> ParsedOptions -> Configurer a -> Either ErrorMessage a
runConfigurer configuration_filepath parsed_options configurer = unsafePerformIO $ do
    either_old_configuration <- 
        fmap (mapLeft (errorMessageText ("parsing configuration file " ++ configuration_filepath) . show)) 
             (readfile emptyCP configuration_filepath)
        `catch`
        (\exception -> return $
            if isDoesNotExistError exception
                then Right emptyCP
                else Left . errorMessageText ("opening " ++ configuration_filepath) . show $ exception
        )
    case either_old_configuration of
        Left error_message -> return (Left error_message)
        Right old_configuration ->  
            let (result,(),A modifyConfiguration) =
                    runRWS
                        (runErrorT configurer)
                        (Environment old_configuration parsed_options)
                        ()
            in do
                writeFile configuration_filepath . to_string . modifyConfiguration $ old_configuration
                return result
-- @-node:gcross.20091128000856.1412:runConfigurer
-- @+node:gcross.20091128000856.1414:cpErrorMessage
cpErrorMessage :: String -> CPError -> ErrorMessage
cpErrorMessage section_name = errorMessageText ("parsing section " ++ section_name) . show
-- @-node:gcross.20091128000856.1414:cpErrorMessage
-- @+node:gcross.20091128000856.1415:configureUsingSection
configureUsingSection ::
    (ConfigurationData a, AutomaticallyConfigurable a) =>
    String ->
    Configurer a
configureUsingSection = configureUsingSectionWith readConfig writeConfig automaticallyConfigure
-- @-node:gcross.20091128000856.1415:configureUsingSection
-- @+node:gcross.20091128201230.1464:configureUsingSectionWith
configureUsingSectionWith ::
    ConfigurationDataReader a ->
    (a -> ConfigurationDataWriter ()) ->
    (ParsedOptions -> Either ErrorMessage a) ->
    String ->
    Configurer a
configureUsingSectionWith
    config_reader
    config_writer
    automatic_configurer
    section_name
    = do
    config_parser <- lift . asks $ environmentConfigParser
    case applyReaderToConfig config_parser section_name config_reader of
        Left cp_error ->
            case fst cp_error of
                NoSection _ -> reconfigure
                NoOption _ -> reconfigure
                _ -> ErrorT (return . Left . cpErrorMessage section_name $ cp_error)
        Right result -> return result
  where
    reconfigure = do
        options_data <- lift . asks $ environmentOptions               
        case automatic_configurer options_data of
            Left error_message -> ErrorT (return . Left $ error_message)
            Right configuration -> do
                tell . A . applyWriterToConfig section_name . config_writer $ configuration
                return configuration
-- @-node:gcross.20091128201230.1464:configureUsingSectionWith
-- @+node:gcross.20091129000542.1569:simpleReadConfig
simpleReadConfig :: ConfigurationKey -> (String -> a) -> ConfigurationDataReader a
simpleReadConfig configuration_key constructor = liftM constructor (getConfig configuration_key)
-- @-node:gcross.20091129000542.1569:simpleReadConfig
-- @+node:gcross.20091129000542.1570:simpleWriteConfig
simpleWriteConfig :: ConfigurationKey -> (a -> String) -> a -> ConfigurationDataWriter ()
simpleWriteConfig configuration_key extractor = setConfig configuration_key . extractor
-- @-node:gcross.20091129000542.1570:simpleWriteConfig
-- @-node:gcross.20091126122246.1379:Functions
-- @-others
-- @-node:gcross.20091123215917.1369:@thin Configuration.hs
-- @-leo
