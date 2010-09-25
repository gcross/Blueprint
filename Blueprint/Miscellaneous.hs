-- @+leo-ver=4-thin
-- @+node:gcross.20100925004153.1323:@thin Miscellaneous.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100925004153.1324:<< Language extensions >>
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100925004153.1324:<< Language extensions >>
-- @nl

module Blueprint.Miscellaneous where

-- @<< Import needed modules >>
-- @+node:gcross.20100925004153.1325:<< Import needed modules >>
import qualified Codec.Binary.UTF8.String as UTF8
import Crypto.Classes

import Data.Digest.Pure.MD5
import Data.Maybe
import Data.Typeable
import Data.UUID
import Data.UUID.V5
-- @-node:gcross.20100925004153.1325:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100925004153.1330:Instances
-- @+node:gcross.20100925004153.1331:Typeable MD5Digest
deriving instance Typeable MD5Digest
-- @-node:gcross.20100925004153.1331:Typeable MD5Digest
-- @-node:gcross.20100925004153.1330:Instances
-- @+node:gcross.20100925004153.1326:Functions
-- @+node:gcross.20100925004153.1327:inNamespace
inNamespace :: UUID → String → UUID
inNamespace uuid =
    generateNamed uuid
    .
    UTF8.encode
-- @-node:gcross.20100925004153.1327:inNamespace
-- @+node:gcross.20100925004153.1328:uuid
uuid :: String → UUID
uuid = fromJust . fromString
-- @-node:gcross.20100925004153.1328:uuid
-- @-node:gcross.20100925004153.1326:Functions
-- @-others
-- @-node:gcross.20100925004153.1323:@thin Miscellaneous.hs
-- @-leo
