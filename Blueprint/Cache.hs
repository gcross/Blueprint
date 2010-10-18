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
    JobIdentifier →
    α →
    (β → Job Bool) →
    Job β →
    Job β
runIfDependencyOrProductHasChanged job_id dependency productHasChangedFrom run =
    cache job_id $ \maybe_cache →
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
-- @+node:gcross.20101005122519.1494:runIfImplicitDependenciesOrProductHasChanged
runIfImplicitDependencyOrProductHasChanged ::
    (Binary α, Eq α, Binary β, Eq β, Eq ɣ, Binary ɣ, Binary δ) ⇒
    JobIdentifier →
    α →
    Job β →
    (β → Job (ɣ,n)) →
    (δ → Job Bool) →
    (ɣ → Job δ) →
    Job (δ,n)
runIfImplicitDependencyOrProductHasChanged
    job_id
    scan_dependency
    scan
    computeDependency
    productHasChangedFrom
    run
  = cache job_id $ \maybe_cache →
        case maybe_cache of
            Just (old_scan_dependency
                 ,old_scan_result
                 ,old_dependency
                 ,old_product
                 ) | old_scan_dependency == scan_dependency → do
                (dependency,miscellaneous) ← computeDependency old_scan_result
                if old_dependency == dependency
                    then do
                        product_has_changed ← productHasChangedFrom old_product
                        if product_has_changed
                            then runIt old_scan_result dependency miscellaneous
                            else return (maybe_cache,(old_product,miscellaneous))
                    else runIt old_scan_result dependency miscellaneous
            _ → do
                scan_result ← scan
                (dependency,miscellaneous) ← computeDependency scan_result
                runIt scan_result dependency miscellaneous
  where
    runIt scan_result dependency miscellaneous = do
        product ← run dependency
        return 
            (Just
                (scan_dependency
                ,scan_result
                ,dependency
                ,product
                )
            ,(product,miscellaneous)
            )
-- @-node:gcross.20101005122519.1494:runIfImplicitDependenciesOrProductHasChanged
-- @-node:gcross.20100925114718.1304:Functions
-- @-others
-- @-node:gcross.20100925114718.1301:@thin Cache.hs
-- @-leo
