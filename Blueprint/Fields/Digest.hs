-- @+leo-ver=4-thin
-- @+node:gcross.20100624100717.2028:@thin Digest.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100624100717.2029:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100624100717.2029:<< Language extensions >>
-- @nl

module Blueprint.Fields.Digest where

-- @<< Import needed modules >>
-- @+node:gcross.20100624100717.2030:<< Import needed modules >>
import Data.Digest.Pure.MD5
import Data.Record
-- @-node:gcross.20100624100717.2030:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100624100717.2031:Fields
-- @+node:gcross.20100624100717.2057:_digest
_digest :: Field MD5Digest
_digest = field "MD5 digest of file" "8f218a7a-9e22-41d2-8aff-ae530f666e86"
-- @-node:gcross.20100624100717.2057:_digest
-- @-node:gcross.20100624100717.2031:Fields
-- @+node:gcross.20100624100717.2058:Functions
-- @+node:gcross.20100624100717.2059:getDigest
getDigest :: FieldValue entity MD5Digest ⇒ Table entity → MD5Digest
getDigest = getRequiredField _digest
-- @-node:gcross.20100624100717.2059:getDigest
-- @-node:gcross.20100624100717.2058:Functions
-- @-others
-- @-node:gcross.20100624100717.2028:@thin Digest.hs
-- @-leo
