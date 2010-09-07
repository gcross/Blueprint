-- @+leo-ver=4-thin
-- @+node:gcross.20100906112631.2212:@thin Configuration.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100906112631.2213:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100906112631.2213:<< Language extensions >>
-- @nl

module Blueprint.Configuration where

-- @<< Import needed modules >>
-- @+node:gcross.20100906112631.2214:<< Import needed modules >>
import Data.Binary
import Data.Typeable
import Data.Monoid

import Blueprint.Jobs
import Blueprint.Jobs.Combinators
import Blueprint.Options
import Blueprint.Phase
import Blueprint.Wrapper
-- @nonl
-- @-node:gcross.20100906112631.2214:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100906112631.2215:Functions
-- @+node:gcross.20100906112631.2216:configurationPhase
configurationPhase ::
    (Ord jobid
    ,Show jobid
    ,Typeable jobid
    ,Binary jobid
    ,Monoid jobid
    ,Wrapper result a
    ) =>
    FilePath →
    FilePath →
    Options →
    (OptionValues → JobApplicative jobid result a) →
    Phase a
configurationPhase
    configuration_filepath
    cache_filepath
    options
    configurer
    =
    Phase $ \is_primary_Phase → do
        options ←
            if is_primary_Phase
                then fmap snd (loadOptions configuration_filepath options)
                else getAndParseConfigurationFile options configuration_filepath
        runJobApplicativeUsingCacheFile 4 cache_filepath
            .
            configurer
            $
            options
-- @nonl
-- @-node:gcross.20100906112631.2216:configurationPhase
-- @-node:gcross.20100906112631.2215:Functions
-- @-others
-- @-node:gcross.20100906112631.2212:@thin Configuration.hs
-- @-leo
