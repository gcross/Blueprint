-- @+leo-ver=4-thin
-- @+node:gcross.20090601155538.2:@thin Options.hs
-- @@language Haskell

module HardHat.Options where

-- @<< Imports >>
-- @+node:gcross.20090601155538.14:<< Imports >>
import Data.ByteString as BS
import Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import System.Console.GetOpt
import System
-- @-node:gcross.20090601155538.14:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20090601155538.3:Types
-- @+node:gcross.20090601155538.12:Option
data Option = Option
    {   optionKey :: String
    ,   optionShortNames :: [Char]
    ,   optionLongNames :: [String]
    ,   optionType :: OptionType
    ,   optionDescription :: [String]
    }
-- @-node:gcross.20090601155538.12:Option
-- @+node:gcross.20090601155538.17:Options
data Options = Options
    {   flagOptions :: Map String Bool
    ,   listOptions :: Map String (Seq BL.ByteString)
    }
-- @-node:gcross.20090601155538.17:Options
-- @+node:gcross.20090601155538.13:OptionType
data OptionType =
        Flag
    |   List String
-- @-node:gcross.20090601155538.13:OptionType
-- @-node:gcross.20090601155538.3:Types
-- @+node:gcross.20090601155538.23:noOptions
noOptions = Options Map.empty Map.empty
-- @-node:gcross.20090601155538.23:noOptions
-- @+node:gcross.20090601155538.18:parseOptions
parseOptions :: [Option] -> IO Options
parseOptions options = do
    args <- getArgs
    let parsed_options :: [Options -> Options]
        (parsed_options,_,_) = getOpt Permute (List.map optionToGetOptOption options) args
        initial_options :: Options
        initial_options = List.foldr ($) noOptions (List.map optionToOptionsInitializer options) --'
    return $ List.foldr ($) initial_options parsed_options
-- @-node:gcross.20090601155538.18:parseOptions
-- @+node:gcross.20090601155538.21:optionToGetOptOption
optionToGetOptOption :: Option -> ArgDescr (Options -> Options)
optionToGetOptOption (Hardhat.Options.Option key short_names long_names option_type description) =
    case option_type of
        Flag -> NoArg (\options -> Options { flagOptions = Map.insert key True optionsFlags options } )
        List argument_type -> ReqArg (\options -> Options { listOptions = Map.insert key True optionsFlags options } )

        (Map.insert key . BL8.pack) option_type
-- @-node:gcross.20090601155538.21:optionToGetOptOption
-- @+node:gcross.20090601155538.22:optionToOptionsInitializer
optionToOptionsInitializer :: Option -> Options -> Options
optionToOptionsInitializer (Hardhat.Options.Option key _ _ option_type _) =
    case option_type of
        Flag -> Map.insert key True
        ArgumentList _ -> Map.insert key Seq.empty

-- @-node:gcross.20090601155538.22:optionToOptionsInitializer
-- @-others
-- @-node:gcross.20090601155538.2:@thin Options.hs
-- @-leo
