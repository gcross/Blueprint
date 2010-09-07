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
import Blueprint.Target
import Blueprint.Wrapper
-- @-node:gcross.20100906112631.2214:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100906112631.2215:Functions
-- @+node:gcross.20100906112631.2216:configurationTarget
configurationTarget ::
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
    Target a
configurationTarget
    configuration_filepath
    cache_filepath
    options
    configurer
    =
    Target $ \is_primary_target → do
        options ←
            if is_primary_target
                then fmap snd (loadOptions configuration_filepath options)
                else getAndParseConfigurationFile options configuration_filepath
        runJobApplicativeUsingCacheFile 4 cache_filepath
            .
            configurer
            $
            options
-- @-node:gcross.20100906112631.2216:configurationTarget
-- @-node:gcross.20100906112631.2215:Functions
-- @-others
-- @-node:gcross.20100906112631.2212:@thin Configuration.hs
-- @-leo
