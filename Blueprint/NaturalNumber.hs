-- @+leo-ver=4-thin
-- @+node:gcross.20100908110213.2002:@thin NaturalNumber.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100908110213.2003:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100908110213.2003:<< Language extensions >>
-- @nl

module Blueprint.NaturalNumber where

-- @<< Import needed modules >>
-- @+node:gcross.20100908110213.2004:<< Import needed modules >>
import Data.Typeable

import TypeLevel.NaturalNumber hiding (NaturalNumber)
-- @-node:gcross.20100908110213.2004:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100908110213.2011:Type families
-- @+node:gcross.20100908110213.2012:Plus
type family Plus m n
type instance Plus Zero n = n
type instance Plus (SuccessorTo m) n = SuccessorTo (Plus m n)
-- @-node:gcross.20100908110213.2012:Plus
-- @-node:gcross.20100908110213.2011:Type families
-- @+node:gcross.20100908110213.2005:Types
-- @+node:gcross.20100908110213.2006:N
data N a where
    NZero :: N Zero
    NSuccessorTo :: N n → N (SuccessorTo n)
  deriving Typeable
-- @-node:gcross.20100908110213.2006:N
-- @-node:gcross.20100908110213.2005:Types
-- @+node:gcross.20100908110213.2013:Classes
-- @+node:gcross.20100908110213.2014:NaturalNumber
class NaturalNumber n where
    asN :: N n
    asInt :: n → Int
-- @-node:gcross.20100908110213.2014:NaturalNumber
-- @-node:gcross.20100908110213.2013:Classes
-- @+node:gcross.20100908110213.2018:Instances
-- @+node:gcross.20100908110213.2019:NaturalNumber n
instance NaturalNumber Zero where
    asN = NZero
    asInt _ = 0
instance NaturalNumber n => NaturalNumber (SuccessorTo n) where
    asN = NSuccessorTo asN
    asInt n = 1 + asInt (predecessorOf n)
-- @-node:gcross.20100908110213.2019:NaturalNumber n
-- @+node:gcross.20100908110213.2020:Typeable n
instance Typeable Zero where
    typeOf n = mkTyConApp (mkTyCon $ "N#0") []
instance NaturalNumber n => Typeable (SuccessorTo n) where
    typeOf n = mkTyConApp (mkTyCon $ "N#" ++ show (asInt n + 1)) []
-- @-node:gcross.20100908110213.2020:Typeable n
-- @-node:gcross.20100908110213.2018:Instances
-- @-others
-- @-node:gcross.20100908110213.2002:@thin NaturalNumber.hs
-- @-leo
