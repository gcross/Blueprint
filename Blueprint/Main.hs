-- @+leo-ver=4-thin
-- @+node:gcross.20091214124713.1626:@thin Main.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091214124713.1630:<< Language extensions >>
-- @-node:gcross.20091214124713.1630:<< Language extensions >>
-- @nl

module Blueprint.Main where

-- @<< Import needed modules >>
-- @+node:gcross.20091214124713.1631:<< Import needed modules >>
import Control.Monad

import Data.ErrorMessage

import System.Environment
import System.Exit
import System.IO

import Text.PrettyPrint.ANSI.Leijen hiding ((</>),(<$>))

import Blueprint.Options
import Blueprint.Targets
-- @-node:gcross.20091214124713.1631:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091214124713.1628:defaultMain
defaultMain :: Doc -> TargetList -> IO ()
defaultMain _ [] = error "There are no targets to build!"
defaultMain help_message targets = do
    args <- getArgs
    when (any isHelpFlag args) $ do
        putDoc help_message
        putStrLn ""
        exitSuccess
    case args of
        [] -> showTargets
        target_name:_ ->
            case lookup target_name targets of
                Nothing -> do
                    putStrLn $ "There is no target named " ++ show target_name
                    putStrLn ""
                    showTargets
                Just target ->
                    case target of
                        Nothing -> putStrLn $ "Completed " ++ target_name ++ "."
                        Just error_message -> do
                            hPutDoc stderr . formatErrorMessage $ error_message
                            hPutStrLn stderr ""
                            exitFailure
  where
    showTargets :: IO ()
    showTargets = do
        putStrLn "Please choose from the following targets:"
        forM_ targets $ \(target_name,_) -> putStrLn ('\t':target_name) --'
-- @-node:gcross.20091214124713.1628:defaultMain
-- @-others
-- @-node:gcross.20091214124713.1626:@thin Main.hs
-- @-leo
