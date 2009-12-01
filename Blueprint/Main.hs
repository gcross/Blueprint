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
import Control.Arrow
import Control.Monad
import Control.Parallel

import Data.Maybe

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
    toTarget :: a -> Target

instance Targetable () where
    toTarget = (`pseq` Nothing)

instance Targetable (Maybe ErrorMessage) where
    toTarget = id

instance Targetable (Either ErrorMessage a) where
    toTarget (Left error_message) = Just error_message
    toTarget (Right _) = Nothing
-- @-node:gcross.20091128000856.1482:Targetable
-- @-node:gcross.20091128000856.1481:Classes
-- @+node:gcross.20091128000856.1479:Types
-- @+node:gcross.20091128000856.1480:Target
type Target = Maybe ErrorMessage
-- @-node:gcross.20091128000856.1480:Target
-- @+node:gcross.20091129000542.1595:TargetList
type TargetList = [(String,Target)]
-- @-node:gcross.20091129000542.1595:TargetList
-- @-node:gcross.20091128000856.1479:Types
-- @+node:gcross.20091128000856.1445:Functions
-- @+node:gcross.20091130193227.2260:target
target :: Targetable a => String -> a -> (String,Target)
target = curry (second toTarget)
-- @-node:gcross.20091130193227.2260:target
-- @+node:gcross.20091128000856.1444:defaultMain
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
-- @-node:gcross.20091128000856.1444:defaultMain
-- @+node:gcross.20091129000542.1596:lookupOldTarget
lookupOldTarget :: String -> String -> TargetList -> Target
lookupOldTarget old_target_name new_target_name =
    fromMaybe (error $ "Programmer error:  The "
                        ++ show new_target_name ++
                        " target cannot find the "
                        ++ show old_target_name ++
                        "  target."
              )
    .
    lookup old_target_name
-- @-node:gcross.20091129000542.1596:lookupOldTarget
-- @+node:gcross.20091130193227.2259:thenTarget
thenTarget :: Target -> Target -> Target
thenTarget a b = maybe b Just a
-- @-node:gcross.20091130193227.2259:thenTarget
-- @+node:gcross.20091130193227.2257:makeTargetFollowing
makeTargetFollowing :: String -> String -> Target -> TargetList -> Target
makeTargetFollowing old_target_name new_target_name new_target =
    maybe new_target Just
    .
    lookupOldTarget old_target_name new_target_name
-- @-node:gcross.20091130193227.2257:makeTargetFollowing
-- @+node:gcross.20091129000542.1594:makeCleanTarget
makeCleanTarget :: [FilePath] -> Target
makeCleanTarget = makeRemoveFilesAndDirectoriesTarget
-- @-node:gcross.20091129000542.1594:makeCleanTarget
-- @+node:gcross.20091129000542.1600:makeDistCleanTarget
makeDistCleanTarget :: [FilePath] -> TargetList -> Target
makeDistCleanTarget =
    makeTargetFollowing "clean" "distclean"
    .
    makeRemoveFilesAndDirectoriesTarget
-- @-node:gcross.20091129000542.1600:makeDistCleanTarget
-- @+node:gcross.20091129000542.1598:makeRebuildTarget
makeRebuildTarget :: TargetList -> Target
makeRebuildTarget old_targets =
    lookupOldTarget "clean" "rebuild" old_targets
    `thenTarget`
    lookupOldTarget "build" "rebuild" old_targets
-- @-node:gcross.20091129000542.1598:makeRebuildTarget
-- @+node:gcross.20091129000542.1592:makeReconfigureTarget
makeReconfigureTarget :: FilePath -> TargetList -> Target
makeReconfigureTarget configuration_filepath old_targets =
    let configure = lookupOldTarget "configure" "reconfigure" old_targets
    in unsafePerformIO $ do
        file_exists <- doesFileExist configuration_filepath
        when file_exists $ removeFile configuration_filepath
        return configure
-- @-node:gcross.20091129000542.1592:makeReconfigureTarget
-- @+node:gcross.20091128000856.1478:removeFilesAndDirectoriesTarget
makeRemoveFilesAndDirectoriesTarget :: [FilePath] -> Target
makeRemoveFilesAndDirectoriesTarget items = unsafePerformIO $
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
    >>
    return Nothing
-- @-node:gcross.20091128000856.1478:removeFilesAndDirectoriesTarget
-- @+node:gcross.20091128000856.1446:targetFromEither
targetFromEither :: Either ErrorMessage a -> Target
targetFromEither (Left error_message) = Just error_message
targetFromEither (Right _) = Nothing
-- @-node:gcross.20091128000856.1446:targetFromEither
-- @-node:gcross.20091128000856.1445:Functions
-- @-others
-- @-node:gcross.20091128000856.1441:@thin Main.hs
-- @-leo
