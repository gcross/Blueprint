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
import Control.Applicative
import Control.Arrow
import Control.Concurrent.MVar
import Control.Monad.IO.Class

import Data.Either.Unwrap
import Data.Function
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.UUID

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
        -- @+node:gcross.20100927123234.1309:once
        ,testCase "once" $ do
            counter ← newIORef 0
            (result,cache) ← withJobEnvironment 2 Map.empty . runJob $ do
                once nil . liftIO $ (modifyIORef counter (+1))
                once nil . liftIO $ (modifyIORef counter (+1))
                return ()
            assertBool "Is the cache empty?" (Map.null cache)
            assertBool "Was an exception thrown?" (isRight $ result)
            readIORef counter >>= assertEqual "Is the counter value correct?" 1
        -- @-node:gcross.20100927123234.1309:once
        -- @+node:gcross.20100927123234.1311:fork
        ,testCase "fork" $ do
            var_1 ← newEmptyMVar
            var_2 ← newEmptyMVar
            (result,cache) ← withJobEnvironment 2 Map.empty . runJob $
                (,) <$> (liftIO (putMVar var_1 42 >> takeMVar var_2))
                    <*> (liftIO (putMVar var_2 24 >> takeMVar var_1))
            assertBool "Is the cache empty?" (Map.null cache)
            assertBool "Was an exception thrown?" (isRight $ result)
            let Right r = result
            assertEqual "Is the result correct?" (24,42) r
        -- @-node:gcross.20100927123234.1311:fork
        -- @-others
        ]
    -- @-node:gcross.20100927123234.1307:runJob
    -- @-others
    -- @-node:gcross.20100927123234.1306:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100924174906.1279:@thin test.hs
-- @-leo
