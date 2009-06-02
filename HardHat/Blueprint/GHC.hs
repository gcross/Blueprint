-- @+leo-ver=4-thin
-- @+node:gcross.20090601155538.31:@thin GHC.hs
-- @@language Haskell

module HardHat.Blueprint.GHC where

-- @<< Imports >>
-- @+node:gcross.20090601155538.39:<< Imports >>
import Control.Monad.Trans
import System.Path

import HardHat.Blueprint
import HardHat.Configuration
import HardHat.Builder
-- @-node:gcross.20090601155538.39:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090601155538.41:HaskellProgram
haskellProgram :: [FilePath] -> Blueprint
haskellProgram sources = do
    liftIO $ putStrLn "entered blueprint"
-- @-node:gcross.20090601155538.41:HaskellProgram
-- @+node:gcross.20090601155538.45:HaskellBuilder
haskellBuilder :: Builder
haskellBuilder = do
    liftIO $ putStr "Called builder"
-- @-node:gcross.20090601155538.45:HaskellBuilder
-- @+node:gcross.20090601155538.42:GHCConfigurer
ghcConfigurer :: Configurer
ghcConfigurer = do
    liftIO $ putStrLn "entered configurer"
-- @-node:gcross.20090601155538.42:GHCConfigurer
-- @-others
-- @-node:gcross.20090601155538.31:@thin GHC.hs
-- @-leo
