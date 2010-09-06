-- @+leo-ver=4-thin
-- @+node:gcross.20100624100717.2122:@thin DeferredDependencies.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100624100717.2123:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100624100717.2123:<< Language extensions >>
-- @nl

module Blueprint.Fields.DeferredDependencies where

-- @<< Import needed modules >>
-- @+node:gcross.20100624100717.2124:<< Import needed modules >>
import Data.Maybe
import Blueprint.Record

import Blueprint.Dependency
-- @nonl
-- @-node:gcross.20100624100717.2124:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100624100717.2140:Fields
-- @+node:gcross.20100624100717.2141:_deferred_dependencies
_deferred_dependencies :: Field [Dependency]
_deferred_dependencies = field "deferred dependencies" "9b9241ac-fd7f-4852-a8b8-113718e15e23"
-- @-node:gcross.20100624100717.2141:_deferred_dependencies
-- @-node:gcross.20100624100717.2140:Fields
-- @+node:gcross.20100624100717.2127:Functions
-- @+node:gcross.20100624100717.2128:getDeferredDependencies
getDeferredDependencies :: FieldValue entity [Dependency] ⇒ Table entity → [Dependency]
getDeferredDependencies = fromMaybe [] . getField _deferred_dependencies
-- @-node:gcross.20100624100717.2128:getDeferredDependencies
-- @+node:gcross.20100906112631.1959:setDeferredDependencies
setDeferredDependencies :: FieldValue entity [Dependency] ⇒ [Dependency] → Table entity → Table entity
setDeferredDependencies = setField _deferred_dependencies
-- @-node:gcross.20100906112631.1959:setDeferredDependencies
-- @+node:gcross.20100708102250.2006:addDeferredDependency
addDeferredDependency :: FieldValue entity [Dependency] ⇒ Dependency → Table entity → Table entity
addDeferredDependency new_dependency record =
    setField _deferred_dependencies (new_dependency:getDeferredDependencies record) record
-- @-node:gcross.20100708102250.2006:addDeferredDependency
-- @-node:gcross.20100624100717.2127:Functions
-- @-others
-- @-node:gcross.20100624100717.2122:@thin DeferredDependencies.hs
-- @-leo
