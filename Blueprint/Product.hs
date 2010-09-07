-- @+leo-ver=4-thin
-- @+node:gcross.20100906112631.2050:@thin Product.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100906112631.2051:<< Language extensions >>
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100906112631.2051:<< Language extensions >>
-- @nl

module Blueprint.Product where

-- @<< Import needed modules >>
-- @+node:gcross.20100906112631.2052:<< Import needed modules >>
import Language.Haskell.TH

import Blueprint.Identifier
import Blueprint.Jobs
import Blueprint.Miscellaneous
-- @-node:gcross.20100906112631.2052:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100906112631.2067:Types
-- @+node:gcross.20100906112631.2070:BuiltProduct
data BuiltProduct a = BuiltProduct
    {   builtProductName :: String
    ,   builtProductJobId :: JobId
    }
-- @-node:gcross.20100906112631.2070:BuiltProduct
-- @+node:gcross.20100906112631.2069:ProductBuilder
type ProductBuilder a = String → BuiltProduct a
-- @-node:gcross.20100906112631.2069:ProductBuilder
-- @-node:gcross.20100906112631.2067:Types
-- @+node:gcross.20100906112631.2064:Functions
-- @+node:gcross.20100906112631.2066:builtProduct
builtProduct :: (String → String → JobId) → String → String → BuiltProduct a
builtProduct jobIdOf product_kind product_name =
    BuiltProduct
        product_name
        (jobIdOf product_name ("Building " ++ product_kind ++ " " ++ product_name))
-- @-node:gcross.20100906112631.2066:builtProduct
-- @+node:gcross.20100906112631.2105:createProductDeclarations
createProductDeclarations :: String → String → Q [Dec]
createProductDeclarations uuid_string name = do
    let namespace_name = mkName (wordsToUnderscores name ++ "_namespace")
        namespace_qexp = return (VarE namespace_name)
        camel_case_name = wordsToCamelCase name
        jobId_name = mkName (camel_case_name ++ "JobId")
        jobId_qexp = return (VarE jobId_name)
        capitalized_name = capitalize camel_case_name
        product_datatype_name = mkName capitalized_name
        product_datatype_qtype = return (ConT product_datatype_name)
        built_name = mkName ("built" ++ capitalized_name)
        name_qexp = return . LitE . StringL $ name
    uuid_exp ← [|uuid uuid_string|]
    jobId_exp ← [|jobIdInNamespace $(namespace_qexp)|]
    built_type ← [t|ProductBuilder $(product_datatype_qtype)|]
    built_exp ←
        [|\product_name →
            BuiltProduct
                product_name
                ($(jobId_qexp) product_name ("Building " ++ $(name_qexp) ++ " " ++ product_name))
        |]
    return
        [ValD (VarP namespace_name) (NormalB uuid_exp) []
        ,ValD (VarP jobId_name) (NormalB jobId_exp) []
        ,emptyType product_datatype_name
        ,SigD built_name built_type
        ,ValD (VarP built_name) (NormalB built_exp) []
        ]
-- @-node:gcross.20100906112631.2105:createProductDeclarations
-- @-node:gcross.20100906112631.2064:Functions
-- @-others
-- @-node:gcross.20100906112631.2050:@thin Product.hs
-- @-leo
