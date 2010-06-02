-- @+leo-ver=4-thin
-- @+node:gcross.20100602141408.1273:@thin Options.hs
-- @@language Haskell
module Options where

-- @+others
-- @+node:gcross.20100602141408.1274:Types
-- @+node:gcross.20100602141408.1275:Option
data Option
    {   optionKey :: String
    ,   optionShortForms :: [Char]
    ,   optionLongForms :: [String]
    ,   optionArgument :: Maybe (Maybe String,String)
    }
-- @-node:gcross.20100602141408.1275:Option
-- @-node:gcross.20100602141408.1274:Types
-- @-others
-- @-node:gcross.20100602141408.1273:@thin Options.hs
-- @-leo
