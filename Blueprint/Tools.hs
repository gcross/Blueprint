-- @+leo-ver=4-thin
-- @+node:gcross.20091201183231.1591:@thin Tools.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091201183231.1593:<< Language extensions >>
-- @-node:gcross.20091201183231.1593:<< Language extensions >>
-- @nl

module Blueprint.Tools where

-- @<< Import needed modules >>
-- @+node:gcross.20091201183231.1594:<< Import needed modules >>
import Control.Arrow
import Control.Monad.Error
import Control.Monad.Trans

import qualified Data.ByteString.Lazy as L
import Data.Either.Unwrap
import Data.ErrorMessage
import Data.List
import Data.Maybe
import qualified Data.Map as Map

import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Text.PrettyPrint.ANSI.Leijen hiding ((</>),(<$>))

import Blueprint.Cache.ImplicitDependencies
import Blueprint.Miscellaneous
import Blueprint.Resources
-- @-node:gcross.20091201183231.1594:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091214092727.1590:Types
-- @+node:gcross.20091214092727.1591:DependencyType
data DependencyType =
    BuildDependency
 |  LinkDependency
 |  BuildAndLinkDependency
-- @-node:gcross.20091214092727.1591:DependencyType
-- @-node:gcross.20091214092727.1590:Types
-- @+node:gcross.20091214092727.1592:Runners
-- @+node:gcross.20091201183231.1592:runProductionCommand
runProductionCommand ::
    String ->
    [FilePath] ->
    [FilePath] ->
    String ->
    [String] ->
    ErrorT ErrorMessage IO ()
runProductionCommand
    error_message_heading
    mandatory_product_filepaths
    optional_product_filepaths
    command
    arguments
  = (liftIO $ do
        putStrLn . unwords . (command:) $ arguments
        mapM_ (createDirectoryIfMissing True . takeDirectory) $
            mandatory_product_filepaths ++ optional_product_filepaths
        readProcessWithExitCode
            command
            arguments
            ""
   )>>= \compilation_result ->
        case compilation_result of
            (ExitFailure _,_,error_message) ->
                throwError $ errorMessageTextFromMultilineString error_message_heading error_message
            (ExitSuccess,_,_) ->
                liftIO (filterM (fmap not . doesFileExist) mandatory_product_filepaths)
                >>=
                \missing_mandatory_files ->
                    if null missing_mandatory_files
                        then return ()
                        else throwError $
                            errorMessage error_message_heading
                            .
                            (text "The following files were supposed to have been produced, but were not:" <$$>)
                            .
                            indent 4
                            .
                            vcat
                            .
                            map text
                            $
                            missing_mandatory_files
-- @-node:gcross.20091201183231.1592:runProductionCommand
-- @+node:gcross.20091214092727.1589:runScanner
runScanner ::
    CompiledRegularExpression ->
    (String -> Maybe [(DependencyType,ResourceId)]) ->
    ((DependencyType,ResourceId) -> Maybe Doc) ->
    Resources ->
    FilePath ->
    Scanner
runScanner
    regex
    postProcess
    messageProcess
    known_resources
    source_filepath
  = ErrorT (
        L.readFile source_filepath
        >>=
        return
        .
        mapBoth
            (
                errorMessage ("tracing the following module dependencies for " ++ source_filepath)
                .
                vcat
                .
                catMaybes
                .
                map messageProcess
            )
            partitionDependencies
        .
        gatherResultsOrErrors
        .
        map (\dependency ->
                if Map.member (snd dependency) known_resources
                    then Right dependency
                    else Left dependency
        )
        .
        concat
        .
        catMaybes
        . 
        map postProcess
        .
        applyRegularExpression regex
    )
-- @nonl
-- @-node:gcross.20091214092727.1589:runScanner
-- @+node:gcross.20100105133009.1609:thenScanner
thenScanner :: Scanner -> Scanner -> Scanner
thenScanner = liftM2 $ \(a,b) (c,d) -> (a++c,b++d)
-- @-node:gcross.20100105133009.1609:thenScanner
-- @-node:gcross.20091214092727.1592:Runners
-- @+node:gcross.20091214092727.1593:Functions
-- @+node:gcross.20091214092727.1594:partitionDependencies
partitionDependencies :: [(DependencyType,a)] -> ([a],[a])
partitionDependencies = foldr (uncurry classifyDependency) ([],[])
  where
    classifyDependency :: DependencyType -> a -> ([a],[a]) -> ([a],[a])
    classifyDependency BuildDependency = first . (:)
    classifyDependency LinkDependency = second . (:)
    classifyDependency BuildAndLinkDependency = uncurry (***) . ((:) &&& (:))
-- @-node:gcross.20091214092727.1594:partitionDependencies
-- @+node:gcross.20091214124713.1588:compileAdditionalWithImplicitDependencies
compileAdditionalWithImplicitDependencies ::
    [String] ->
    (Resources -> Resource -> [Resource]) ->
    Resources ->
    Resources ->
    Resources
compileAdditionalWithImplicitDependencies
    accepted_resource_types
    compile
    source_resources
    previously_compiled_resources
    =
    let new_resources =
            go (Map.elems source_resources)
               (source_resources `Map.union` previously_compiled_resources)
        go [] = id
        go (resource:rest_resources) =
            go rest_resources
            .
            if resourceType (resource) `elem` accepted_resource_types
                then
                    flip (foldl' (flip addResource)) -- '
                    $
                    compile new_resources resource
                else id
    in new_resources
-- @-node:gcross.20091214124713.1588:compileAdditionalWithImplicitDependencies
-- @-node:gcross.20091214092727.1593:Functions
-- @-others
-- @-node:gcross.20091201183231.1591:@thin Tools.hs
-- @-leo
