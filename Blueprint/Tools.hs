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
import Control.Monad.Error
import Control.Monad.Trans

import Data.ErrorMessage

import System.Directory
import System.Exit
import System.FilePath
import System.Process

-- @-node:gcross.20091201183231.1594:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091201183231.1592:runProductionCommand
runProductionCommand :: String -> [FilePath] -> String -> [String] -> ErrorT ErrorMessage IO ()
runProductionCommand error_message_heading files_being_produced command arguments =
    (liftIO $ do
        putStrLn . unwords . (command:) $ arguments
        mapM_ (createDirectoryIfMissing True . takeDirectory) $ files_being_produced
        readProcessWithExitCode
            command
            arguments
            ""
   )>>= \compilation_result ->
        case compilation_result of
            (ExitFailure _,_,error_message) ->
                throwError $ errorMessageTextFromMultilineString error_message_heading error_message
            (ExitSuccess,_,_) -> return ()
-- @nonl
-- @-node:gcross.20091201183231.1592:runProductionCommand
-- @-others
-- @-node:gcross.20091201183231.1591:@thin Tools.hs
-- @-leo
