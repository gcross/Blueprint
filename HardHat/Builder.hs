-- @+leo-ver=4-thin
-- @+node:gcross.20090530015605.3:@thin Builder.hs
-- @@language Haskell

module HardHat.Builder where

-- @<< Imports >>
-- @+node:gcross.20090530015605.4:<< Imports >>
import Prelude hiding (lookup)

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans
import Data.IVar.Simple (IVar)
import qualified Data.IVar.Simple as IVar
import Data.Map
import System.Directory
import System.IO.Unsafe
import System.Time
-- @-node:gcross.20090530015605.4:<< Imports >>
-- @nl

-- @<< Types >>
-- @+node:gcross.20090530015605.8:<< Types >>
-- @+others
-- @+node:gcross.20090530015605.9:BuildContext
data BuildContext = BuildContext
    {   fileHasChangedTable :: TVar (Map FilePath Bool)
    ,   fileModificationTimeCache :: Map FilePath CalendarTime
    -- ,   configurationTable :: Map String Dynamic
    }
-- @-node:gcross.20090530015605.9:BuildContext
-- @+node:gcross.20090601155538.38:BuildMonad
type BuildMonad = ReaderT BuildContext IO
-- @-node:gcross.20090601155538.38:BuildMonad
-- @+node:gcross.20090530015605.10:Builder
type Builder = BuildMonad ()
-- @-node:gcross.20090530015605.10:Builder
-- @-others
-- @-node:gcross.20090530015605.8:<< Types >>
-- @nl

-- @+others
-- @+node:gcross.20090530015605.11:queryIfFileHasChanged
queryIfFileHasChanged :: FilePath -> BuildMonad Bool
queryIfFileHasChanged filepath = do
    table_tvar <- asks fileHasChangedTable
    modification_times <- asks fileModificationTimeCache
    liftIO $ do
        ivar <- lazyNewIVar
        maybe_status <- atomically $ do
            table <- readTVar table_tvar
            case lookup filepath table of
                Just status -> return $ Just status
                Nothing -> do
                    writeTVar table_tvar (insert filepath (IVar.read ivar) table)
                    return Nothing
        case maybe_status of
            Just status -> return status
            Nothing -> do
                status <-
                    case lookup filepath modification_times of
                        Nothing -> return True
                        Just last_modification_time ->
                            getModificationTime filepath
                            >>=
                            return . (/= last_modification_time) . toUTCTime
                IVar.write ivar status
                return status
-- @-node:gcross.20090530015605.11:queryIfFileHasChanged
-- @+node:gcross.20090530015605.12:queryIfAnyFileHasChangedIn
queryIfAnyFileHasChangedIn :: [FilePath] -> BuildMonad Bool
queryIfAnyFileHasChangedIn filepaths = mapM queryIfFileHasChanged filepaths >>= return . or
-- @-node:gcross.20090530015605.12:queryIfAnyFileHasChangedIn
-- @+node:gcross.20090601155538.27:lazyNewIVar
lazyNewIVar = unsafeInterleaveIO $ IVar.new :: IO (IVar Bool)
-- @-node:gcross.20090601155538.27:lazyNewIVar
-- @-others
-- @-node:gcross.20090530015605.3:@thin Builder.hs
-- @-leo
