-- @+leo-ver=4-thin
-- @+node:gcross.20100924174906.1279:@thin test.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20100927123234.1305:<< Language extensions >>
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100927123234.1305:<< Language extensions >>
-- @nl

-- @<< Import needed modules >>
-- @+node:gcross.20100927123234.1304:<< Import needed modules >>
import Control.Arrow

import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map

import System.IO.Unsafe

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Blueprint.Cache
import Blueprint.Job
import Blueprint.Miscellaneous
import Blueprint.Tools
-- @-node:gcross.20100927123234.1304:<< Import needed modules >>
-- @nl

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20100927123234.1306:<< Tests >>
    -- @+others
    -- @+node:gcross.20100927123234.1307:runJob
    [testGroup "runJob" $
        -- @    @+others
        -- @+node:gcross.20100927123234.1308:return
        [testProperty "return" $
            \(x :: Int) →
                uncurry (&&)
                .
                (either (const False) (== x) *** Map.null)
                .
                unsafePerformIO
                .
                withJobEnvironment 0 Map.empty
                .
                runJob
                .
                return
                $
                x
        -- @-node:gcross.20100927123234.1308:return
        -- @-others
        ]
    -- @-node:gcross.20100927123234.1307:runJob
    -- @-others
    -- @-node:gcross.20100927123234.1306:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100924174906.1279:@thin test.hs
-- @-leo
