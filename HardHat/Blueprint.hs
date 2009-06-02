-- @+leo-ver=4-thin
-- @+node:gcross.20090601155538.4:@thin Blueprint.hs
-- @@language Haskell

module HardHat.Blueprint where

-- @<< Imports >>
-- @+node:gcross.20090601155538.15:<< Imports >>
import Control.Monad.Reader
import Control.Monad.Trans
import Data.IORef
import Data.Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import HardHat.Builder
import HardHat.Configuration
-- @nonl
-- @-node:gcross.20090601155538.15:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090601155538.8:Types
-- @+node:gcross.20090601155538.9:BlueprintContext
data BlueprintContext = BlueprintContext
    {    targetFilepaths :: IORef (Map FilePath Bool)
    ,    activeConfigurers :: IORef (Map String Configurer)
    ,    builders :: IORef (Seq Builder)
    }
-- @-node:gcross.20090601155538.9:BlueprintContext
-- @+node:gcross.20090601155538.26:BlueprintMonad
type BlueprintMonad = ReaderT BlueprintContext IO
-- @-node:gcross.20090601155538.26:BlueprintMonad
-- @+node:gcross.20090601155538.40:Blueprint
type Blueprint = BlueprintMonad ()
-- @-node:gcross.20090601155538.40:Blueprint
-- @-node:gcross.20090601155538.8:Types
-- @-others
-- @-node:gcross.20090601155538.4:@thin Blueprint.hs
-- @-leo
