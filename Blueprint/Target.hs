-- @+leo-ver=4-thin
-- @+node:gcross.20100906112631.2173:@thin Target.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100906112631.2174:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100906112631.2174:<< Language extensions >>
-- @nl

module Blueprint.Target where

-- @<< Import needed modules >>
-- @+node:gcross.20100906112631.2175:<< Import needed modules >>
-- @-node:gcross.20100906112631.2175:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100906112631.2176:Types
-- @+node:gcross.20100906112631.2177:Target
newtype Target a = Target { unwrapTarget :: Bool → IO a }
-- @-node:gcross.20100906112631.2177:Target
-- @-node:gcross.20100906112631.2176:Types
-- @+node:gcross.20100906112631.2178:Instances
-- @+node:gcross.20100906112631.2179:Monad Target
instance Monad Target where
    return = Target . const . return
    Target x >>= f = Target $ \is_primary_target → do
        x_result ← x False
        unwrapTarget (f x_result) is_primary_target
    Target x >> Target y = Target $ (x False >>) . y
-- @-node:gcross.20100906112631.2179:Monad Target
-- @-node:gcross.20100906112631.2178:Instances
-- @+node:gcross.20100906112631.2192:Functions
-- @+node:gcross.20100906112631.2193:runTarget
runTarget :: Target a → IO ()
runTarget (Target x) = x True >> return ()
-- @-node:gcross.20100906112631.2193:runTarget
-- @-node:gcross.20100906112631.2192:Functions
-- @-others
-- @-node:gcross.20100906112631.2173:@thin Target.hs
-- @-leo
