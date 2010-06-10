-- @+leo-ver=4-thin
-- @+node:gcross.20100609163522.1415:@thin Object.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100609163522.1416:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100609163522.1416:<< Language extensions >>
-- @nl

module Data.Object where

-- @<< Import needed modules >>
-- @+node:gcross.20100609163522.1417:<< Import needed modules >>
import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Binary
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Data
import Data.Dynamic
import Data.Maybe
import Data.Monoid
import Data.Trie (Trie)
import qualified Data.Trie as Trie
import Data.Typeable
import qualified Data.UUID as UUID
import Data.Vec ((:.)(..))

import Debug.Trace
-- @-node:gcross.20100609163522.1417:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100609163522.1434:Exceptions
-- @+node:gcross.20100609163522.1435:TypeError
data TypeError = ∀ a. Typeable a => TypeError (Field a) deriving (Typeable)

instance Eq TypeError

instance Show TypeError where
    show (TypeError (Field{..} :: Field a)) =
        "TypeError:  Unable to cast field named " ++ fieldName

instance Exception TypeError
-- @-node:gcross.20100609163522.1435:TypeError
-- @-node:gcross.20100609163522.1434:Exceptions
-- @+node:gcross.20100609163522.1438:Classes
-- @+node:gcross.20100609163522.1439:Castable
class Castable a where
    toObject :: a → Object
    fromObject :: Object → Maybe a
-- @-node:gcross.20100609163522.1439:Castable
-- @+node:gcross.20100609163522.1440:FieldList
class FieldList fields result where
    type FieldListArgument fields result
    fromObjectUsingFields :: fields → FieldListArgument fields result → Object → Maybe result
    toObjectUsingFields :: fields → result → Object

instance FieldList () result where
    type FieldListArgument () result = result
    fromObjectUsingFields _ result _ = Just result
    toObjectUsingFields _ _ = Object (Trie.empty)

instance (Binary field_type, Typeable field_type, FieldList rest_fields value) => FieldList ((Field field_type,value → field_type) :. rest_fields) value where
    type FieldListArgument ((Field field_type,value → field_type) :. rest_fields) value = field_type → FieldListArgument rest_fields value
    fromObjectUsingFields ((field,_) :. rest_fields) f object =
        case getField field object of
            Nothing → Nothing
            Just value → fromObjectUsingFields rest_fields (f value) object
    toObjectUsingFields ((field,getter) :. rest_fields) x =
        setField field (getter x) (toObjectUsingFields rest_fields x)
-- @-node:gcross.20100609163522.1440:FieldList
-- @+node:gcross.20100609163522.1732:FieldsAndValues
class FieldsAndValues field_and_values where
    setFields :: field_and_values -> Object -> Object

instance FieldsAndValues () where
    setFields _ object = object

instance (Binary a, Typeable a, FieldsAndValues rest_fields) => FieldsAndValues ((Field a,a) :. rest_fields) where
    setFields ((field,value) :. rest_fields) object = setFields rest_fields (setField field value object)

-- @-node:gcross.20100609163522.1732:FieldsAndValues
-- @+node:gcross.20100609163522.1734:Fields
class Fields fields where
    type FieldValues fields
    getFields :: fields -> Object -> FieldValues fields

instance Fields () where
    type FieldValues () = ()
    getFields _ _ = ()

instance (Binary a, Typeable a, Fields rest_fields) => Fields (Field a :. rest_fields) where
    type FieldValues (Field a :. rest_fields) = Maybe a :. FieldValues rest_fields
    getFields (field :. rest_fields) object = getField field object :. getFields rest_fields object
-- @-node:gcross.20100609163522.1734:Fields
-- @-node:gcross.20100609163522.1438:Classes
-- @+node:gcross.20100609163522.1418:Types
-- @+node:gcross.20100609163522.1419:Entity
data Entity =
    Entity
    {   entityValue :: Dynamic
    ,   entitySerialized :: L.ByteString
    }
  | SerializedEntity
    {   entityData :: L.ByteString
    ,   entityType :: String
    }
-- @-node:gcross.20100609163522.1419:Entity
-- @+node:gcross.20100609163522.1433:Object
newtype Object = Object (Trie Entity) deriving (Typeable)
-- @-node:gcross.20100609163522.1433:Object
-- @+node:gcross.20100609163522.1422:Field
data Field a = Field
    {   fieldName :: String
    ,   fieldId :: S.ByteString
    } deriving (Show,Eq)
-- @-node:gcross.20100609163522.1422:Field
-- @-node:gcross.20100609163522.1418:Types
-- @+node:gcross.20100609163522.1695:Instances
-- @+node:gcross.20100609163522.1696:Monoid Object
instance Monoid Object where
    mempty = Object (Trie.empty)
    Object x `mappend` Object y = Object (Trie.unionR x y)
-- @-node:gcross.20100609163522.1696:Monoid Object
-- @+node:gcross.20100609163522.1736:Binary Object
instance Binary Object where
    put (Object fields) = put fields
    get = fmap Object get
-- @-node:gcross.20100609163522.1736:Binary Object
-- @+node:gcross.20100609163522.1738:Binary Entity
instance Binary Entity where
    put (Entity{..}) = do
        put entitySerialized
        put . show . dynTypeRep $ entityValue
    get = liftM2 SerializedEntity get get
-- @-node:gcross.20100609163522.1738:Binary Entity
-- @-node:gcross.20100609163522.1695:Instances
-- @+node:gcross.20100609163522.1425:Functions
-- @+node:gcross.20100609163522.1426:toEntity
toEntity :: (Binary a, Typeable a) => a → Entity
toEntity =
    Entity
        <$> toDyn
        <*> encode
-- @-node:gcross.20100609163522.1426:toEntity
-- @+node:gcross.20100609163522.1427:fromEntity
fromEntity :: (Binary a, Typeable a) => Entity → Maybe a
fromEntity (Entity { entityValue }) = fromDynamic entityValue
fromEntity (SerializedEntity {..}) =
    let x = decode entityData
    in if entityType == show (typeOf x)
        then Just x
        else Nothing
-- @-node:gcross.20100609163522.1427:fromEntity
-- @+node:gcross.20100609163522.1430:getField
getField :: (Binary a, Typeable a) => Field a → Object → Maybe a
getField (field@Field {..}) (Object fields) =
    case Trie.lookup fieldId fields of
        Nothing → Nothing
        Just entity →
            case fromEntity entity of
                Nothing → throw (TypeError field)
                Just value → Just value
-- @-node:gcross.20100609163522.1430:getField
-- @+node:gcross.20100609163522.1432:setField
setField :: (Binary a, Typeable a) => Field a → a → Object → Object
setField (Field {..}) new_value (Object fields) =
    Object (Trie.insert fieldId (toEntity new_value) fields)
-- @-node:gcross.20100609163522.1432:setField
-- @+node:gcross.20100609163522.1693:field
field :: String → String → Field a
field name = Field name . S.concat . L.toChunks . UUID.toByteString . fromJust . UUID.fromString
-- @nonl
-- @-node:gcross.20100609163522.1693:field
-- @+node:gcross.20100609163522.1694:updateObjectWith
updateObjectWith :: Castable a => a → Object → Object
updateObjectWith x = (`mappend` toObject x)
-- @-node:gcross.20100609163522.1694:updateObjectWith
-- @+node:gcross.20100609203325.1472:withFields
withFields :: FieldsAndValues fields => fields -> Object
withFields fields = setFields fields emptyObject
-- @-node:gcross.20100609203325.1472:withFields
-- @-node:gcross.20100609163522.1425:Functions
-- @+node:gcross.20100609163522.1698:Values
-- @+node:gcross.20100609163522.1700:emptyObject
emptyObject = mempty :: Object
-- @-node:gcross.20100609163522.1700:emptyObject
-- @-node:gcross.20100609163522.1698:Values
-- @-others
-- @-node:gcross.20100609163522.1415:@thin Object.hs
-- @-leo
