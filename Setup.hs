-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1291:@thin Setup.hs
-- @@language Haskell

import Blueprint.Resources

main = do
    putStrLn . show $ resourcesIn "src"
-- @-node:gcross.20091121210308.1291:@thin Setup.hs
-- @-leo
