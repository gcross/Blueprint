-- @+leo-ver=4-thin
-- @+node:gcross.20100611224425.1700:@thin Compilers.hs
-- @@language Haskell
-- @<< Language extensions >>
-- @+node:gcross.20100611224425.1701:<< Language extensions >>
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
-- @-node:gcross.20100611224425.1701:<< Language extensions >>
-- @nl

module Blueprint.Tools.Compilers where

-- @<< Import needed modules >>
-- @+node:gcross.20100611224425.1702:<< Import needed modules >>
import Control.Arrow
import Control.Monad
import Control.Exception

import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as S
import Data.Data
import Data.Object
import Data.Typeable

import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Text.StringTemplate

import Blueprint.Language
import Blueprint.Miscellaneous
-- @-node:gcross.20100611224425.1702:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20100611224425.1703:Types
-- @+node:gcross.20100614121927.1665:CompilationException
data CompilationException = CompilationException String deriving (Show,Typeable)

instance Exception CompilationException
-- @-node:gcross.20100614121927.1665:CompilationException
-- @+node:gcross.20100611224425.1704:Compiler
data Compiler language = Compiler
    {   compilerProgram :: FilePath
    ,   compilerInvocationToCompileObject :: StringTemplate String
    ,   compilerInvocationToCompileProgram :: StringTemplate String
    ,   compilationLibraryDependencies :: [Library]
    }
-- @-node:gcross.20100611224425.1704:Compiler
-- @+node:gcross.20100614121927.1637:Library
data Library = Library
    {   libraryName :: String
    ,   libraryLocation :: Maybe FilePath
    ,   libraryDescription :: Maybe String
    } deriving (Typeable,Data)
-- @-node:gcross.20100614121927.1637:Library
-- @+node:gcross.20100614121927.1644:Script
newtype Script language = Script S.ByteString deriving Typeable
-- @-node:gcross.20100614121927.1644:Script
-- @+node:gcross.20100614121927.1653:SourceCodeFile
newtype SourceCodeFile language = SourceCodeFile { unwrapSourceCodeFile ::  FilePath }
-- @-node:gcross.20100614121927.1653:SourceCodeFile
-- @-node:gcross.20100611224425.1703:Types
-- @+node:gcross.20100611224425.1715:Functions
-- @+node:gcross.20100614121927.1666:computeCompileToProgramCommand
computeCompileToProgramCommand :: Compiler language → [SourceCodeFile language] → FilePath → (FilePath,[String])
computeCompileToProgramCommand compiler sources program =
    (head &&& tail)
    .
    words
    .
    toString
    .
    setAttribute "program" program
    .
    setAttribute "sources" (map unwrapSourceCodeFile sources)
    .
    compilerInvocationToCompileProgram
    $
    compiler
-- @-node:gcross.20100614121927.1666:computeCompileToProgramCommand
-- @+node:gcross.20100611224425.1716:compilerFieldForLanguage
compilerFieldForLanguage :: Language language ⇒ language → Field (Compiler language)
compilerFieldForLanguage language =
    Field ("compiler for " ++ languageName language)
    .
    uuidInNamespace compiler_namespace
    .
    languageUUID
    $
    language
-- @-node:gcross.20100611224425.1716:compilerFieldForLanguage
-- @+node:gcross.20100614121927.1652:compileToProgram
compileToProgram :: Compiler language → SourceCodeFile language → FilePath → IO (Maybe String)
compileToProgram compiler source program =
    (uncurry readProcessWithExitCode $ computeCompileToProgramCommand compiler [source] program) ""
    >>=
    \(exit_code,stdout,stderr) → return $
        case exit_code of
            ExitSuccess → Nothing
            ExitFailure _ → Just (stdout ++ stderr)
-- @-node:gcross.20100614121927.1652:compileToProgram
-- @+node:gcross.20100614121927.1651:runScript
runScript :: Language language ⇒ Compiler language → Script language → IO String
runScript compiler script =
    withScriptAsFile script $ \script_file →
    withTemporaryFile "" $ \program →
        compileToProgram compiler script_file program
        >>=
        \compilation_result →
            case compilation_result of
                Just errors → throwIO $ CompilationException errors
                Nothing → readProcess program [] ""
-- @-node:gcross.20100614121927.1651:runScript
-- @+node:gcross.20100614121927.1650:scriptFromLines
scriptFromLines :: [String] -> Script language
scriptFromLines = Script . pack . unlines
-- @-node:gcross.20100614121927.1650:scriptFromLines
-- @+node:gcross.20100614121927.1654:withScriptAsFile
withScriptAsFile :: Language language ⇒ Script language → (SourceCodeFile language → IO a) → IO a
withScriptAsFile script@(Script script_data) thunk =
    withTemporaryFile (languageFileExtension . languageOf $ script) $ \filepath →
        S.writeFile filepath script_data
        >>
        thunk (SourceCodeFile filepath)
-- @-node:gcross.20100614121927.1654:withScriptAsFile
-- @-node:gcross.20100611224425.1715:Functions
-- @+node:gcross.20100611224425.1718:Values
-- @+node:gcross.20100611224425.1719:compiler_namespace
compiler_namespace = uuid "a3b4b965-9801-43bd-a0fd-9c8ea5699035"
-- @-node:gcross.20100611224425.1719:compiler_namespace
-- @-node:gcross.20100611224425.1718:Values
-- @-others
-- @-node:gcross.20100611224425.1700:@thin Compilers.hs
-- @-leo
