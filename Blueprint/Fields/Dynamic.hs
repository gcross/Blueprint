-- @+leo-ver=4-thin
-- @+node:gcross.20100902094127.2086:@thin Dynamic.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100902094127.2087:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100902094127.2087:<< Language extensions >>
-- @nl

module Blueprint.Fields.Dynamic where

-- @<< Import needed modules >>
-- @+node:gcross.20100902094127.2088:<< Import needed modules >>
import Data.Dynamic
import Blueprint.Record
-- @nonl
-- @-node:gcross.20100902094127.2088:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100902094127.2089:Fields
-- @+node:gcross.20100902094127.2090:_dynamic
_dynamic :: Field Dynamic
_dynamic = field "Dynamic" "e6e67fdb-deea-451c-9c31-aff66ce835d2"
-- @nonl
-- @-node:gcross.20100902094127.2090:_dynamic
-- @-node:gcross.20100902094127.2089:Fields
-- @+node:gcross.20100902094127.2091:Functions
-- @+node:gcross.20100902094127.2092:getDynamic
getDynamic :: FieldValue entity Dynamic ⇒ Table entity → Dynamic
getDynamic = getRequiredField _dynamic
-- @-node:gcross.20100902094127.2092:getDynamic
-- @+node:gcross.20100902094127.2094:setDynamic
setDynamic :: FieldValue entity Dynamic ⇒ Dynamic → Table entity → Table entity
setDynamic = setField _dynamic
-- @-node:gcross.20100902094127.2094:setDynamic
-- @-node:gcross.20100902094127.2091:Functions
-- @-others
-- @-node:gcross.20100902094127.2086:@thin Dynamic.hs
-- @-leo
