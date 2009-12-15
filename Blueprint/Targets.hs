-- @+leo-ver=4-thin
-- @+node:gcross.20091214124713.1651:@thin Targets.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091214124713.1652:<< Language extensions >>
{-# LANGUAGE FlexibleInstances #-}
-- @-node:gcross.20091214124713.1652:<< Language extensions >>
-- @nl

module Blueprint.Targets where

-- @<< Import needed modules >>
-- @+node:gcross.20091214124713.1653:<< Import needed modules >>
import Control.Applicative.Infix
import Control.Arrow
import Control.Monad
import Control.Parallel

import Data.Maybe
import Data.ErrorMessage

import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe
import System.Process

import Text.PrettyPrint.ANSI.Leijen

import Blueprint.Options
import Blueprint.Resources
-- @-node:gcross.20091214124713.1653:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091214124713.1654:Classes
-- @+node:gcross.20091214124713.1655:Targetable
class Targetable a where
    toTarget :: a -> Target

instance Targetable () where
    toTarget = (`pseq` Nothing)

instance Targetable (Maybe ErrorMessage) where
    toTarget = id

instance Targetable (Either ErrorMessage a) where
    toTarget (Left error_message) = Just error_message
    toTarget (Right _) = Nothing
-- @-node:gcross.20091214124713.1655:Targetable
-- @-node:gcross.20091214124713.1654:Classes
-- @+node:gcross.20091214124713.1656:Types
-- @+node:gcross.20091214124713.1657:Target
type Target = Maybe ErrorMessage
-- @-node:gcross.20091214124713.1657:Target
-- @+node:gcross.20091214124713.1658:TargetList
type TargetList = [(String,Target)]
-- @-node:gcross.20091214124713.1658:TargetList
-- @-node:gcross.20091214124713.1656:Types
-- @+node:gcross.20091214124713.1659:Functions
-- @+node:gcross.20091214124713.1660:target
target :: Targetable a => String -> a -> (String,Target)
target = curry (second toTarget)
-- @-node:gcross.20091214124713.1660:target
-- @+node:gcross.20091214124713.1661:lookupOldTarget
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
-- @-node:gcross.20091214124713.1661:lookupOldTarget
-- @+node:gcross.20091214124713.1662:thenTarget
thenTarget :: (Targetable a, Targetable b) => a -> b -> Target
thenTarget a b = maybe (toTarget b) Just (toTarget a)
-- @-node:gcross.20091214124713.1662:thenTarget
-- @+node:gcross.20091214124713.1671:targetFromEither
targetFromEither :: Either ErrorMessage a -> Target
targetFromEither (Left error_message) = Just error_message
targetFromEither (Right _) = Nothing
-- @-node:gcross.20091214124713.1671:targetFromEither
-- @-node:gcross.20091214124713.1659:Functions
-- @+node:gcross.20091214124713.1672:Targets
-- @+node:gcross.20091214124713.1663:makeTargetFollowing
makeTargetFollowing :: Targetable t => String -> String -> t -> TargetList -> Target
makeTargetFollowing old_target_name new_target_name new_target =
    maybe (toTarget new_target) Just
    .
    lookupOldTarget old_target_name new_target_name
-- @-node:gcross.20091214124713.1663:makeTargetFollowing
-- @+node:gcross.20091214124713.1664:makeCleanTarget
makeCleanTarget :: [FilePath] -> Target
makeCleanTarget = makeRemoveFilesAndDirectoriesTarget
-- @-node:gcross.20091214124713.1664:makeCleanTarget
-- @+node:gcross.20091214124713.1665:makeDistCleanTarget
makeDistCleanTarget :: [FilePath] -> TargetList -> Target
makeDistCleanTarget =
    makeTargetFollowing "clean" "distclean"
    .
    makeRemoveFilesAndDirectoriesTarget
-- @-node:gcross.20091214124713.1665:makeDistCleanTarget
-- @+node:gcross.20091214124713.1666:makeRebuildTarget
makeRebuildTarget :: TargetList -> Target
makeRebuildTarget old_targets =
    lookupOldTarget "clean" "rebuild" old_targets
    `thenTarget`
    lookupOldTarget "build" "rebuild" old_targets
-- @-node:gcross.20091214124713.1666:makeRebuildTarget
-- @+node:gcross.20091214124713.1667:makeReconfigureTarget
makeReconfigureTarget :: FilePath -> TargetList -> Target
makeReconfigureTarget configuration_filepath old_targets =
    let configure = lookupOldTarget "configure" "reconfigure" old_targets
    in unsafePerformIO $ do
        file_exists <- doesFileExist configuration_filepath
        when file_exists $ removeFile configuration_filepath
        return configure
-- @-node:gcross.20091214124713.1667:makeReconfigureTarget
-- @+node:gcross.20091214124713.1668:makeRemoveFilesAndDirectoriesTarget
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
-- @-node:gcross.20091214124713.1668:makeRemoveFilesAndDirectoriesTarget
-- @+node:gcross.20091214124713.1679:makeRunTestTarget
makeRunTestTarget :: ErrorMessageOr Resource -> ErrorMessageOr ()
makeRunTestTarget possibly_test_program_resource =
    possibly_test_program_resource
    >>=
    \test_program_resource ->
        unsafePerformIO (system . resourceFilePath $ test_program_resource)
        `pseq`
        return ()
-- @-node:gcross.20091214124713.1679:makeRunTestTarget
-- @+node:gcross.20091214124713.1689:makeRetestTarget
makeRetestTarget :: [FilePath] -> TargetList -> Target
makeRetestTarget directories_to_clean old_targets =
    makeRemoveFilesAndDirectoriesTarget directories_to_clean
    `thenTarget`
    lookupOldTarget "test" "retest" old_targets
-- @-node:gcross.20091214124713.1689:makeRetestTarget
-- @-node:gcross.20091214124713.1672:Targets
-- @-others
-- @-node:gcross.20091214124713.1651:@thin Targets.hs
-- @-leo
