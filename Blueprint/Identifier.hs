-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Blueprint.Identifier where

-- Imports {{{
import qualified Codec.Binary.UTF8.String as UTF8

import Control.Monad

import Data.Binary
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Data.UUID
import qualified Data.UUID as UUID
import Data.UUID.V5 (generateNamed)

import Blueprint.Miscellaneous
-- }}}

-- Types {{{
data Identifier α = Identifier
    {   identifierUUID :: !UUID
    ,   identifierDescription :: !String
    } deriving (Typeable)
-- }}}

-- Instances {{{
instance Binary (Identifier α) where -- {{{
    put (Identifier α β) = do { put α; put β }
    get = liftM2 Identifier get get
-- }}}
instance Eq (Identifier α) where -- {{{
    (==) = (==) `on` identifierUUID
-- }}}
instance Ord (Identifier α) where -- {{{
    compare = compare `on` identifierUUID
-- }}}
instance Show (Identifier α) where -- {{{
    show = identifierDescription
-- }}}
instance Monoid (Identifier α) where -- {{{
    mempty = Identifier nil ""
    i1 `mappend` i2
      | UUID.null (identifierUUID i1) = i2
      | UUID.null (identifierUUID i2) = i1
      | otherwise =
            identifierInNamespace
                (identifierUUID i1)
                (identifierDescription i1 ++ ", " ++ identifierDescription i2)
-- }}}
-- }}}

-- Functions {{{
identifier :: String → String → Identifier α -- {{{
identifier = Identifier . uuid
-- }}}
identifierInNamespace :: UUID → String → Identifier α -- {{{
identifierInNamespace namespace name =
    identifierInNamespaceWithDifferentDisplayName namespace name name
-- }}}
identifierInNamespaceWithDifferentDisplayName :: UUID → String → String → Identifier α -- {{{
identifierInNamespaceWithDifferentDisplayName namespace name display_name =
    Identifier
        (generateNamed namespace . UTF8.encode $ name)
        display_name
-- }}}
sortIntoLabeledBins :: [(Identifier α, β)] → Map (Identifier α) [β] -- {{{
sortIntoLabeledBins =
    foldr
        (\(k,v) bins →
            flip (Map.insert k) bins
            .
            maybe [v] (v:)
            .
            Map.lookup k
            $
            bins
        )
        Map.empty
-- }}}
getContentsOfBinLabeledBy :: Identifier α → Map (Identifier α) [β] → [β] -- {{{
getContentsOfBinLabeledBy key = fromMaybe [] . Map.lookup key
-- }}}
-- }}}

-- Values {{{
null_identifier :: Identifier α
null_identifier = Identifier UUID.nil ""
-- }}}
