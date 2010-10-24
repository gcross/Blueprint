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

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Either.Unwrap
import Data.Function
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.UUID as UUID

import System.IO.Unsafe

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Blueprint.Cache
import Blueprint.Configuration
import Blueprint.Identifier
import Blueprint.Job
import Blueprint.Main ()
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Tools
import Blueprint.Tools.Ar
import Blueprint.Tools.GHC hiding (defaultMain)
-- @-node:gcross.20100927123234.1304:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100927123234.1416:Generators
-- @+node:gcross.20100927123234.1417:Arbitrary UUID
instance Arbitrary UUID where
    arbitrary = fmap (fromJust . UUID.fromByteString . L.pack) . vectorOf 16 $ arbitrary
-- @-node:gcross.20100927123234.1417:Arbitrary UUID
-- @-node:gcross.20100927123234.1416:Generators
-- @+node:gcross.20100927123234.1411:Values
-- @+node:gcross.20100927123234.1413:Test identifiers
test_identifier = identifier "1fa9d5fa-0b71-4e59-9534-6c7d2c146717" "test identifier"
test_identifier_1 = identifier "4b33b60f-09c1-416e-a421-d048aca91699" "test identifier 1"
test_identifier_2 = identifier "d3e9ab2c-1430-466e-a451-906f98031c22" "test identifier 2"
test_identifier_3 = identifier "fc0a9f7c-b2b6-4baa-b399-c608dc6bc41f" "test identifier 3"
test_identifier_4 = identifier "09693966-f6b8-4aa1-938f-5a4897b7b5a6" "test identifier 4"
test_identifier_5 = identifier "87eadc97-8580-4b01-87b4-e707eb10a51f" "test identifier 5"
-- @-node:gcross.20100927123234.1413:Test identifiers
-- @-node:gcross.20100927123234.1411:Values
-- @-others

main = defaultMain
    -- @    << Tests >>
    -- @+node:gcross.20100927123234.1306:<< Tests >>
    -- @+others
    -- @+node:gcross.20100927123234.1406:Blueprint.Identifier
    [testGroup "Blueprint.Identifier" $
        -- @    @+others
        -- @+node:gcross.20100927123234.1407:sortIntoLabeledBins
        [testGroup "sortIntoLabeledBins" $
            -- @    @+others
            -- @+node:gcross.20100927123234.1408:trivial
            [testCase "trivial" $
                assertEqual
                    "Is the resulting map correct?"
                    (Map.empty :: Map (Identifier ()) [()])
                .
                sortIntoLabeledBins
                $
                []
            -- @-node:gcross.20100927123234.1408:trivial
            -- @+node:gcross.20100927123234.1409:singleton
            ,testCase "singleton" $
                assertEqual
                    "Is the resulting map correct?"
                    (Map.singleton test_identifier [()])
                .
                sortIntoLabeledBins
                $
                [(test_identifier,())]
            -- @-node:gcross.20100927123234.1409:singleton
            -- @+node:gcross.20100927123234.1410:random data
            ,testProperty "random data" $ do
                identifiers ←
                    choose (1,10)
                    >>=
                    fmap (zipWith (\index uuid → Identifier uuid ("uuid #" ++ show index)) [1..])
                    .
                    flip vectorOf arbitrary
                random_data ←
                    choose (1,100)
                    >>=
                    flip vectorOf (liftA2 (,) (elements identifiers) (arbitrary :: Gen Int))
                let binned_data = sortIntoLabeledBins random_data
                return
                    .
                    all (
                        \identifier →
                            (map snd . filter ((== identifier) . fst) $ random_data)
                            ==
                            fromMaybe [] (Map.lookup identifier binned_data)
                    )

                    $
                    identifiers
            -- @-node:gcross.20100927123234.1410:random data
            -- @-others
            ]
        -- @-node:gcross.20100927123234.1407:sortIntoLabeledBins
        -- @-others
        ]
    -- @-node:gcross.20100927123234.1406:Blueprint.Identifier
    -- @+node:gcross.20100927123234.1400:Blueprint.Job
    ,testGroup "Blueprint.Job" $
        -- @    @+others
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
                    runJob 0 Map.empty
                    .
                    return
                    $
                    x
            -- @-node:gcross.20100927123234.1308:return
            -- @+node:gcross.20100927123234.1309:once
            ,testCase "once" $ do
                counter ← newIORef 0
                (result,cache) ← runJob 2 Map.empty $ do
                    once null_identifier . liftIO $ (modifyIORef counter (+1))
                    once null_identifier . liftIO $ (modifyIORef counter (+1))
                    return ()
                assertBool "Is the cache empty?" (Map.null cache)
                assertBool "Was an exception thrown?" (isRight $ result)
                readIORef counter >>= assertEqual "Is the counter value correct?" 1
            -- @-node:gcross.20100927123234.1309:once
            -- @+node:gcross.20100927123234.1311:fork
            ,testCase "fork" $ do
                var_1 ← newEmptyMVar
                var_2 ← newEmptyMVar
                (result,cache) ← runJob 2 Map.empty $
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
        ]
    -- @-node:gcross.20100927123234.1400:Blueprint.Job
    -- @-others
    -- @-node:gcross.20100927123234.1306:<< Tests >>
    -- @nl
    ]
-- @-node:gcross.20100924174906.1279:@thin test.hs
-- @-leo
