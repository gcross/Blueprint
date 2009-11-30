-- @+leo-ver=4-thin
-- @+node:gcross.20091128000856.1441:@thin Main.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091128000856.1442:<< Language extensions >>
{-# LANGUAGE FlexibleInstances #-}
-- @-node:gcross.20091128000856.1442:<< Language extensions >>
-- @nl

module Blueprint.Main where

-- @<< Import needed modules >>
-- @+node:gcross.20091128000856.1443:<< Import needed modules >>
import Control.Applicative.Infix
import Control.Monad
import Control.Parallel

import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe

import Text.PrettyPrint.ANSI.Leijen

import Blueprint.Error
import Blueprint.Options
-- @-node:gcross.20091128000856.1443:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091128000856.1481:Classes
-- @+node:gcross.20091128000856.1482:Targetable
class Targetable a where
    target :: String -> a -> (String,Target)

instance Targetable () where
    target name value = (name,value `pseq` Nothing)

instance Targetable (Maybe ErrorMessage) where
    target name value = (name,value)

instance Targetable (Either ErrorMessage a) where
    target name value =
        (name
        ,case value of
            Left error_message -> Just error_message
            Right _ -> Nothing
        )
-- @-node:gcross.20091128000856.1482:Targetable
-- @-node:gcross.20091128000856.1481:Classes
-- @+node:gcross.20091128000856.1479:Types
-- @+node:gcross.20091128000856.1480:Target
type Target = Maybe ErrorMessage
-- @-node:gcross.20091128000856.1480:Target
-- @-node:gcross.20091128000856.1479:Types
-- @+node:gcross.20091128000856.1445:Functions
-- @+node:gcross.20091128000856.1444:defaultMain
defaultMain :: Doc -> [(String,Target)] -> IO ()
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
-- @-node:gcross.20091128000856.1444:defaultMain
-- @+node:gcross.20091128000856.1446:targetFromEither
targetFromEither :: Either ErrorMessage a -> Target
targetFromEither (Left error_message) = Just error_message
targetFromEither (Right _) = Nothing
-- @-node:gcross.20091128000856.1446:targetFromEither
-- @+node:gcross.20091128000856.1478:removeFilesAndDirectoriesTarget
removeFilesAndDirectoriesTarget :: [FilePath] -> ()
removeFilesAndDirectoriesTarget items = unsafePerformIO $
    forM_ items (\item ->
        (liftM2 (,) (doesDirectoryExist item) (doesFileExist item))
        >>=
        (\(directory_exists,file_exists) ->
            case (directory_exists,file_exists) of
                (True,_) -> do
                    putStrLn . ("Removing directory " ++) . show $ item
                    removeDirectoryRecursive item
                (_,True) -> do
                    putStrLn . ("Removing file " ++) . show $ item
                    removeFile item
                _ -> return ()
        )
    )
-- @-node:gcross.20091128000856.1478:removeFilesAndDirectoriesTarget
-- @-node:gcross.20091128000856.1445:Functions
-- @-others
-- @-node:gcross.20091128000856.1441:@thin Main.hs
-- @-leo
