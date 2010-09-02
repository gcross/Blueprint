-- @+leo-ver=4-thin
-- @+node:gcross.20100624100717.2089:@thin FilePath.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100624100717.2090:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100624100717.2090:<< Language extensions >>
-- @nl

module Blueprint.Fields.FilePath where

-- @<< Import needed modules >>
-- @+node:gcross.20100624100717.2091:<< Import needed modules >>
import Data.Record
-- @-node:gcross.20100624100717.2091:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100624100717.2092:Fields
-- @+node:gcross.20100624100717.2093:_file_path
_file_path :: Field FilePath
_file_path = field "file path" "15487351-6cfe-46a8-9d60-cdfcbb1aebfe"
-- @-node:gcross.20100624100717.2093:_file_path
-- @-node:gcross.20100624100717.2092:Fields
-- @+node:gcross.20100624100717.2094:Functions
-- @+node:gcross.20100624100717.2095:getFilePath
getFilePath :: FieldValue entity FilePath ⇒ Table entity → FilePath
getFilePath = getRequiredField _file_path
-- @-node:gcross.20100624100717.2095:getFilePath
-- @+node:gcross.20100901221002.2072:setFilePath
setFilePath :: FieldValue entity FilePath ⇒ FilePath → Table entity → Table entity
setFilePath = setField _file_path
-- @-node:gcross.20100901221002.2072:setFilePath
-- @-node:gcross.20100624100717.2094:Functions
-- @-others
-- @-node:gcross.20100624100717.2089:@thin FilePath.hs
-- @-leo
