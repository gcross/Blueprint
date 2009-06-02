-- @+leo-ver=4-thin
-- @+node:gcross.20090601155538.32:@thin Configuration.hs
-- @@language Haskell

module HardHat.Configuration where

-- @<< Imports >>
-- @+node:gcross.20090601155538.33:<< Imports >>
-- import Hardhat.Options
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.IVar.Simple
import Data.Map

-- @-node:gcross.20090601155538.33:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090601155538.34:Types
-- @+node:gcross.20090601155538.35:ConfigurationContext
data ConfigurationContext = ConfigurationContext
    --{    confOptions :: Options
    {    confSearchPaths :: TVar (Map String [FilePath])
    ,    confPrograms :: TVar (Map String FilePath)
    ,    confConfigurerStatuses :: TVar (Map String (IVar ()))
    }
-- @-node:gcross.20090601155538.35:ConfigurationContext
-- @+node:gcross.20090601155538.36:ConfigurationMonad
type ConfigurationMonad = ReaderT ConfigurationContext IO
-- @-node:gcross.20090601155538.36:ConfigurationMonad
-- @+node:gcross.20090601155538.37:Configurer
type Configurer = ConfigurationMonad ()
-- @-node:gcross.20090601155538.37:Configurer
-- @-node:gcross.20090601155538.34:Types
-- @-others
-- @-node:gcross.20090601155538.32:@thin Configuration.hs
-- @-leo
