-- @+leo-ver=4-thin
-- @+node:gcross.20091121210308.1291:@thin Setup.hs
-- @@language Haskell

import Blueprint.Resources
import Blueprint.Tools.GHC

main = do
    putStrLn . show $ resourcesIn "src"
    putStrLn . show $ ghcTools
-- @-node:gcross.20091121210308.1291:@thin Setup.hs
-- @-leo
