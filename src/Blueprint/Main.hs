-- @+leo-ver=4-thin
-- @+node:gcross.20091128000856.1441:@thin Main.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091128000856.1442:<< Language extensions >>
-- @-node:gcross.20091128000856.1442:<< Language extensions >>
-- @nl

module Blueprint.Main where

-- @<< Import needed modules >>
-- @+node:gcross.20091128000856.1443:<< Import needed modules >>
import Control.Monad

import System.Environment

import Text.PrettyPrint.ANSI.Leijen

import Blueprint.Error
-- @-node:gcross.20091128000856.1443:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091128000856.1445:Functions
-- @+node:gcross.20091128000856.1444:defaultMain
defaultMain :: [(String,Maybe ErrorMessage)] -> IO ()
defaultMain [] = error "There are no targets to build!"
defaultMain targets = do
    args <- getArgs
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
                        Just error_message -> putDoc . formatErrorMessage $ error_message
  where
    showTargets :: IO ()
    showTargets = do
        putStrLn "Please choose from the following targets:"
        forM_ targets $ \(target_name,_) -> putStrLn ('\t':target_name) --'
-- @-node:gcross.20091128000856.1444:defaultMain
-- @+node:gcross.20091128000856.1446:targetFromEither
targetFromEither :: Either ErrorMessage a -> Maybe ErrorMessage
targetFromEither (Left error_message) = Just error_message
targetFromEither (Right _) = Nothing
-- @-node:gcross.20091128000856.1446:targetFromEither
-- @+node:gcross.20091128000856.1447:target
target :: String -> Either ErrorMessage a -> (String,Maybe ErrorMessage)
target target_name target_value = (target_name,targetFromEither target_value)
-- @-node:gcross.20091128000856.1447:target
-- @-node:gcross.20091128000856.1445:Functions
-- @-others
-- @-node:gcross.20091128000856.1441:@thin Main.hs
-- @-leo
