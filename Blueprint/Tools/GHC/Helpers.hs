-- @+leo-ver=4-thin
-- @+node:gcross.20091214124713.1597:@thin Helpers.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091214124713.1598:<< Language extensions >>
-- @-node:gcross.20091214124713.1598:<< Language extensions >>
-- @nl

module Blueprint.Tools.GHC.Helpers where

-- @<< Import needed modules >>
-- @+node:gcross.20091214124713.1599:<< Import needed modules >>
import Control.Applicative
import Control.Monad

import Data.ErrorMessage
import Data.Function
import qualified Data.Map as Map

import Distribution.Package (PackageIdentifier(..),PackageName(..),Dependency(..))
import Distribution.PackageDescription (PackageDescription)

import System.FilePath

import Blueprint.Configuration
import Blueprint.Miscellaneous
import Blueprint.Resources
import Blueprint.Tools.Ar
import Blueprint.Tools.GHC
import Blueprint.Tools.Ld
import Blueprint.Tools.Installer
-- @-node:gcross.20091214124713.1599:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091214124713.1601:Types
-- @+node:gcross.20091214124713.1603:Configuration
data Configuration = Configuration
    {   ghcConfiguration :: GHCConfiguration
    ,   arConfiguration :: ArConfiguration
    ,   ldConfiguration :: LdConfiguration
    ,   installerConfiguration :: InstallerConfiguration
    ,   packageDependencies :: [String]
    ,   packageModules :: PackageModules
    }
-- @-node:gcross.20091214124713.1603:Configuration
-- @-node:gcross.20091214124713.1601:Types
-- @+node:gcross.20091214124713.1613:Values
-- @+node:gcross.20091214124713.1614:build roots
programBuildRoot = "build" </> "programs"
libraryBuildRoot = "build" </> "library"
-- @-node:gcross.20091214124713.1614:build roots
-- @+node:gcross.20091214124713.1706:ghc_options
ghc_options =
    [   installerOptions
    ,   arOptions
    ,   ldOptions
    ,   ghcOptions
    ]
-- @-node:gcross.20091214124713.1706:ghc_options
-- @-node:gcross.20091214124713.1613:Values
-- @+node:gcross.20091214124713.1680:Configurers
-- @+node:gcross.20091214124713.1600:makeCustomizedConfigurer
makeCustomizedConfigurer :: String -> String -> [Dependency] -> Configurer Configuration
makeCustomizedConfigurer package_resolution_section package_cache_section package_dependencies =
 do configurations@
        (ghc_configuration
        ,ar_configuration
        ,ld_configuration
        ,install_configuration
        ) <- (,,,)
            <$> (configureUsingSection "GHC")
            <*> (configureUsingSection "Binutils")
            <*> (configureUsingSection "Binutils")
            <*> (configureUsingSection "Installation Directories")
    package_resolutions <- configurePackageResolutions ghc_configuration package_dependencies package_resolution_section
    package_modules <- configurePackageModules ghc_configuration package_resolutions package_cache_section
    return $
        Configuration
            ghc_configuration
            ar_configuration
            ld_configuration
            install_configuration
            package_resolutions
            package_modules
-- @-node:gcross.20091214124713.1600:makeCustomizedConfigurer
-- @+node:gcross.20091214124713.1682:makeTestConfigurer
makeTestConfigurer :: [Dependency] -> [String] -> Configurer Configuration
makeTestConfigurer dependencies =
    makeCustomizedConfigurer
        "Test Packages"
        "ZZZ - Test Package Module Cache - Please do not edit this unless you know what you are doing."
    .
    (++ dependencies)
    .
    map parseDependency
-- @-node:gcross.20091214124713.1682:makeTestConfigurer
-- @+node:gcross.20091214124713.1684:makeConfigurer
makeConfigurer :: [Dependency] -> Configurer Configuration
makeConfigurer =
    makeCustomizedConfigurer
        "GHC"
        "ZZZ - Package Module Cache - Please do not edit this unless you know what you are doing."
-- @-node:gcross.20091214124713.1684:makeConfigurer
-- @-node:gcross.20091214124713.1680:Configurers
-- @+node:gcross.20091214124713.1604:Functions
-- @+node:gcross.20091214124713.1700:buildTestProgram
buildTestProgram :: Configuration -> [String] -> Resources -> ErrorMessageOr Resource
buildTestProgram configuration ghc_flags =
    assertResourceExists
    .
    ghcLinkProgram
       (ghcConfiguration configuration)
       (programBuildRoot </> "digest-cache")
       ghc_flags
       (packageDependencies configuration)
       "."
       [("test","o")]
    .
    compileObjectsForProgram
        configuration
        ghc_flags
-- @-node:gcross.20091214124713.1700:buildTestProgram
-- @+node:gcross.20091214124713.1607:compileObjectsWithRoot
compileObjectsWithRoot :: String -> Configuration -> [String] -> Resources -> Resources
compileObjectsWithRoot build_root configuration flags source_resources  =
    ghcCompileAll
        (ghcConfiguration configuration)
        (build_root </> "digest-cache")
        flags
        (packageModules configuration)
        (build_root </> "objects")
        (build_root </> "haskell-interfaces")
        source_resources
-- @-node:gcross.20091214124713.1607:compileObjectsWithRoot
-- @+node:gcross.20091214124713.1612:compileObjectsFor___
compileObjectsForLibrary :: Configuration -> String -> [String] -> Resources -> Resources
compileObjectsForLibrary configuration qualified_package_name flags =
    compileObjectsWithRoot libraryBuildRoot configuration (("-package-name="++qualified_package_name):flags)

compileObjectsForProgram :: Configuration -> [String] -> Resources -> Resources
compileObjectsForProgram =
    compileObjectsWithRoot programBuildRoot
-- @-node:gcross.20091214124713.1612:compileObjectsFor___
-- @+node:gcross.20091214124713.1696:installLibrary
installLibrary :: PackageDescription -> Configuration -> [Resource] -> ErrorMessageOr ()
installLibrary package_description configuration =
    liftMaybeErrorToEither
    .
    installSimplePackage
        (ghcConfiguration configuration)
        (installerConfiguration configuration)
        package_description
        (packageDependencies configuration)
-- @-node:gcross.20091214124713.1696:installLibrary
-- @+node:gcross.20091214124713.1609:linkLibrary
linkLibrary ::
    Configuration ->
    String ->
    Resources ->
    ErrorMessageOr [Resource]
linkLibrary
    configuration
    package_name
    compiled_resources 
 = do
    library <-
        assertResourceExists
        $
        formStaticLibrary
            (arConfiguration configuration)
            cache_subdirectory
            object_resources
            ("lib" ++ package_name)
            (library_subdirectory </> "lib" ++ package_name <.> "a")
    ghci_library <-
        assertResourceExists
        $
        linkIntoObject
            (ldConfiguration configuration)
            cache_subdirectory
            object_resources
            package_name
            (library_subdirectory </> package_name <.> "o")
    return (library:ghci_library:interface_resources)
  where
    object_subdirectory = libraryBuildRoot </> "objects"
    interface_subdirectory = libraryBuildRoot </> "haskell-interfaces"
    library_subdirectory = libraryBuildRoot </> "libraries"
    cache_subdirectory = libraryBuildRoot </> "digest-cache"
    object_resources = selectResourcesInSubdirectoryAsList object_subdirectory compiled_resources
    interface_resources = selectResourcesInSubdirectoryAsList interface_subdirectory compiled_resources
-- @-node:gcross.20091214124713.1609:linkLibrary
-- @-node:gcross.20091214124713.1604:Functions
-- @-others
-- @-node:gcross.20091214124713.1597:@thin Helpers.hs
-- @-leo
