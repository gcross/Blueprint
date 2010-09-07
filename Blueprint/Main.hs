-- @+leo-ver=4-thin
-- @+node:gcross.20100906112631.2204:@thin Main.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100906112631.2205:<< Language extensions >>
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100906112631.2205:<< Language extensions >>
-- @nl

module Blueprint.Main where

-- @<< Import needed modules >>
-- @+node:gcross.20100906112631.2206:<< Import needed modules >>
import System.Environment
import System.Log.Logger
-- @-node:gcross.20100906112631.2206:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100906112631.2207:Functions
-- @+node:gcross.20100906112631.2208:runMain
runMain :: [(String,IO ())] → IO ()
runMain commands = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    args ← getArgs
    let command = unwords . takeWhile ((/= '-') . head) $ args
    case lookup command commands of
        Just action → action
        Nothing → do
            putStrLn $ "Unrecognized command: '" ++ command ++ "'"
            putStrLn $ ""
            putStrLn $ "The available commands are:"
            mapM_ (putStrLn . ('\t':) . fst) commands -- '
-- @-node:gcross.20100906112631.2208:runMain
-- @-node:gcross.20100906112631.2207:Functions
-- @-others
-- @-node:gcross.20100906112631.2204:@thin Main.hs
-- @-leo
