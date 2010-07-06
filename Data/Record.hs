-- @+leo-ver=4-thin
-- @+node:gcross.20100609163522.1415:@thin Record.hs
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

module Data.Record where

-- @<< Import needed modules >>
-- @+node:gcross.20100609163522.1417:<< Import needed modules >>
import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Binary
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Data
import Data.Dynamic
import Data.Maybe
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V5 (generateNamed)
import Data.Vec ((:.)(..))
-- @-node:gcross.20100609163522.1417:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100609163522.1434:Exceptions
-- @+node:gcross.20100624100717.2062:FieldNotFoundError
data FieldNotFoundError = ∀ a. Typeable a => FieldNotFoundError (Field a) deriving (Typeable)

instance Eq FieldNotFoundError

instance Show FieldNotFoundError where
    show (FieldNotFoundError (Field{..} :: Field a)) =
        "Field " ++ fieldName ++ " does not exist."

instance Exception FieldNotFoundError
-- @-node:gcross.20100624100717.2062:FieldNotFoundError
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
-- @+node:gcross.20100609163522.1734:Fields
class Fields entity fields where
    type FieldValues fields
    getFields :: fields -> Table entity -> FieldValues fields

instance Typeable entity => Fields entity () where
    type FieldValues () = ()
    getFields _ _ = ()

instance (FieldValue entity value, Fields entity rest_fields) => Fields entity (Field value :. rest_fields) where
    type FieldValues (Field value :. rest_fields) = Maybe value :. FieldValues rest_fields
    getFields (field :. rest_fields) table = getField field table :. getFields rest_fields table
-- @-node:gcross.20100609163522.1734:Fields
-- @+node:gcross.20100609163522.1732:FieldsAndValues
class Typeable entity => FieldsAndValues entity field_and_values where
    setFields :: field_and_values -> Table entity -> Table entity

instance Typeable entity => FieldsAndValues entity () where
    setFields _ table = table

instance (FieldValue entity value
         ,FieldsAndValues entity rest_fields
         ) => FieldsAndValues entity ((Field value,value) :. rest_fields)
  where
    setFields ((field,value) :. rest_fields) table = setFields rest_fields (setField field value table)
-- @-node:gcross.20100609163522.1732:FieldsAndValues
-- @+node:gcross.20100609223718.1500:FieldValue
class (Typeable entity, Typeable value) => FieldValue entity value where
    toEntity :: value -> entity
    fromEntity :: entity -> Maybe value
-- @-node:gcross.20100609223718.1500:FieldValue
-- @+node:gcross.20100609163522.1439:Castable
class Typeable entity => Castable entity cls where
    toRecord :: cls → Table entity
    fromRecord :: Table entity → Maybe cls
-- @-node:gcross.20100609163522.1439:Castable
-- @+node:gcross.20100609163522.1440:RecordFields
class RecordFields entity fields record where
    type RecordBuilder fields record
    fromRecordUsingFields :: fields → RecordBuilder fields record → Table entity → Maybe record
    toRecordUsingFields :: fields → record → Table entity

instance Typeable entity => RecordFields entity () record where
    type RecordBuilder () record = record
    fromRecordUsingFields _ result _ = Just result
    toRecordUsingFields _ _ = Table (Map.empty)

instance (FieldValue entity value
         ,RecordFields entity rest_fields record
         ) => RecordFields entity ((Field value,record → value) :. rest_fields) record
  where
    type RecordBuilder ((Field value,record → value) :. rest_fields) record =
            value → RecordBuilder rest_fields record
    fromRecordUsingFields ((field,_) :. rest_fields) f table =
        case getField field table of
            Nothing → Nothing
            Just value → fromRecordUsingFields rest_fields (f value) table
    toRecordUsingFields ((field,getter) :. rest_fields) x =
        setField field (getter x) (toRecordUsingFields rest_fields x)
-- @nonl
-- @-node:gcross.20100609163522.1440:RecordFields
-- @-node:gcross.20100609163522.1438:Classes
-- @+node:gcross.20100609163522.1418:Types
-- @+node:gcross.20100609163522.1419:Entity
data Entity =
    Entity
    {   entityValue :: Dynamic
    ,   entitySerialized :: ByteString
    }
  | SerializedEntity
    {   entityData :: ByteString
    ,   entityType :: String
    } deriving (Typeable)
-- @-node:gcross.20100609163522.1419:Entity
-- @+node:gcross.20100609163522.1422:Field
data Field value = Field
    {   fieldName :: String
    ,   fieldId :: UUID
    } deriving (Show,Eq)
-- @-node:gcross.20100609163522.1422:Field
-- @+node:gcross.20100609163522.1433:Table
newtype Typeable entity => Table entity = Table (Map UUID entity) deriving (Typeable)
-- @-node:gcross.20100609163522.1433:Table
-- @+node:gcross.20100609223718.1504:Record/SerializableRecord
type Record = Table Dynamic
type SerializableRecord = Table Entity
-- @nonl
-- @-node:gcross.20100609223718.1504:Record/SerializableRecord
-- @-node:gcross.20100609163522.1418:Types
-- @+node:gcross.20100609163522.1695:Instances
-- @+node:gcross.20100609163522.1696:Monoid Table
instance Typeable entity => Monoid (Table entity) where
    mempty = Table mempty
    Table x `mappend` Table y = Table (y `mappend` x)
-- @-node:gcross.20100609163522.1696:Monoid Table
-- @+node:gcross.20100609163522.1736:Binary Table
instance (Binary entity, Typeable entity) => Binary (Table entity) where
    put (Table fields) = put . Map.toList $ fields
    get = fmap (Table . Map.fromList) get
-- @-node:gcross.20100609163522.1736:Binary Table
-- @+node:gcross.20100609163522.1738:Binary Entity
instance Binary Entity where
    put (Entity{..}) = do
        put entitySerialized
        put . show . dynTypeRep $ entityValue
    put (SerializedEntity x y) = put x >> put y
    get = liftM2 SerializedEntity get get
-- @-node:gcross.20100609163522.1738:Binary Entity
-- @+node:gcross.20100609223718.1501:FieldValue Dynamic
instance Typeable value => FieldValue Dynamic value where
    toEntity = toDyn
    fromEntity = fromDynamic
-- @-node:gcross.20100609223718.1501:FieldValue Dynamic
-- @+node:gcross.20100609223718.1503:FieldValue Entity
instance (Binary value, Typeable value) => FieldValue Entity value where
    toEntity =
        Entity
            <$> toDyn
            <*> encode

    fromEntity (Entity { entityValue }) = fromDynamic entityValue
    fromEntity (SerializedEntity {..}) =
        let x = decode entityData
        in if entityType == show (typeOf x)
            then Just x
            else Nothing
-- @-node:gcross.20100609223718.1503:FieldValue Entity
-- @-node:gcross.20100609163522.1695:Instances
-- @+node:gcross.20100609163522.1425:Functions
-- @+node:gcross.20100609163522.1430:getField
getField ::
    (FieldValue entity value
    ,Typeable entity
    ) =>
    Field value →
    Table entity →
    Maybe value
getField (field@Field {..}) (Table fields) =
    case Map.lookup fieldId fields of
        Nothing → Nothing
        Just entity →
            case fromEntity entity of
                Nothing → throw (TypeError field)
                Just value → Just value
-- @-node:gcross.20100609163522.1430:getField
-- @+node:gcross.20100624100717.2061:getRequiredField
getRequiredField ::
    (FieldValue entity value
    ,Typeable entity
    ) =>
    Field value →
    Table entity →
    value
getRequiredField field = fromMaybe (throw (FieldNotFoundError field)) . getField field
-- @-node:gcross.20100624100717.2061:getRequiredField
-- @+node:gcross.20100609163522.1432:setField
setField ::
    (FieldValue entity value
    ,Typeable entity
    ) =>
    Field value →
    value →
    Table entity →
    Table entity
setField (Field {..}) new_value (Table fields) =
    Table (Map.insert fieldId (toEntity new_value) fields)
-- @-node:gcross.20100609163522.1432:setField
-- @+node:gcross.20100609163522.1693:field
field :: String → String → Field value
field name = Field name . uuid
-- @-node:gcross.20100609163522.1693:field
-- @+node:gcross.20100609163522.1694:updateRecordWith
updateRecordWith :: (Typeable entity, Castable entity record) => record → Table entity → Table entity
updateRecordWith x = (`mappend` toRecord x)
-- @-node:gcross.20100609163522.1694:updateRecordWith
-- @+node:gcross.20100609203325.1472:withFields
withFields :: FieldsAndValues entity fields => fields -> Table entity
withFields fields = setFields fields mempty
-- @-node:gcross.20100609203325.1472:withFields
-- @+node:gcross.20100705185804.1967:withField
withField ::
    (FieldValue entity value
    ,Typeable entity
    ) =>
    Field value →
    value →
    Table entity
withField field value = setField field value emptyTable
-- @-node:gcross.20100705185804.1967:withField
-- @+node:gcross.20100611224425.1724:uuid
uuid :: String → UUID
uuid = fromJust . UUID.fromString
-- @-node:gcross.20100611224425.1724:uuid
-- @+node:gcross.20100611224425.1717:uuidInNamespace
uuidInNamespace :: UUID → UUID → UUID
uuidInNamespace namespace = generateNamed namespace . L.unpack . UUID.toByteString
-- @-node:gcross.20100611224425.1717:uuidInNamespace
-- @-node:gcross.20100609163522.1425:Functions
-- @+node:gcross.20100609163522.1698:Values
-- @+node:gcross.20100609163522.1700:emptyTable
emptyTable :: Typeable entity => Table entity
emptyTable = mempty
-- @-node:gcross.20100609163522.1700:emptyTable
-- @-node:gcross.20100609163522.1698:Values
-- @-others
-- @-node:gcross.20100609163522.1415:@thin Record.hs
-- @-leo
