-- @+leo-ver=4-thin
-- @+node:gcross.20100925114718.1301:@thin Cache.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100925114718.1302:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100925114718.1302:<< Language extensions >>
-- @nl

module Blueprint.Cache where

-- @<< Import needed modules >>
-- @+node:gcross.20100925114718.1303:<< Import needed modules >>
import Data.Binary
import Data.UUID

import Blueprint.Job
-- @-node:gcross.20100925114718.1303:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100925114718.1304:Functions
-- @+node:gcross.20100925114718.1310:runIfDependencyOrProductHasChanged
runIfDependencyOrProductHasChanged ::
    (Binary α, Eq α, Binary β) ⇒
    UUID →
    α →
    (β → Job Bool) →
    Job β →
    Job β
runIfDependencyOrProductHasChanged uuid dependency productHasChangedFrom run =
    cache uuid $ \maybe_cache →
        case maybe_cache of
            Just (old_dependency,old_product) | old_dependency == dependency → do
                product_has_changed ← productHasChangedFrom old_product
                if product_has_changed
                    then runIt
                    else return (Just (dependency,old_product),old_product)
            _ → runIt
  where
    runIt = do
        product ← run
        return (Just (dependency,product),product)
-- @-node:gcross.20100925114718.1310:runIfDependencyOrProductHasChanged
-- @-node:gcross.20100925114718.1304:Functions
-- @-others
-- @-node:gcross.20100925114718.1301:@thin Cache.hs
-- @-leo
