-- @+leo-ver=4-thin
-- @+node:gcross.20100830091258.2004:@thin Tools.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100830091258.2005:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100830091258.2005:<< Language extensions >>
-- @nl

module Blueprint.Configuration.Tools where

-- @<< Import needed modules >>
-- @+node:gcross.20100830091258.2006:<< Import needed modules >>
import Control.Exception
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Typeable
import Data.Version

import System.Directory
import System.Environment
import System.FilePath
import System.Process

import Text.Regex.Base

import Blueprint.Miscellaneous
-- @nonl
-- @-node:gcross.20100830091258.2006:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100905161144.1938:Exceptions
-- @+node:gcross.20100905161144.1939:BadProgramVersionException
data BadProgramVersionException = BadProgramVersionException FilePath String deriving (Show,Eq,Typeable)

instance Exception BadProgramVersionException
-- @-node:gcross.20100905161144.1939:BadProgramVersionException
-- @-node:gcross.20100905161144.1938:Exceptions
-- @+node:gcross.20100830091258.2007:Functions
-- @+node:gcross.20100905161144.1941:determineProgramVersion
determineProgramVersion ::
    (String → Maybe Version) →
    [String] →
    FilePath →
    IO Version
determineProgramVersion tryParseVersion arguments program =
    readProcess program arguments ""
    >>=
    \output →
        case tryParseVersion output of
            Nothing → throwIO $ BadProgramVersionException program output
            Just version → return version
-- @-node:gcross.20100905161144.1941:determineProgramVersion
-- @-node:gcross.20100830091258.2007:Functions
-- @-others
-- @-node:gcross.20100830091258.2004:@thin Tools.hs
-- @-leo
