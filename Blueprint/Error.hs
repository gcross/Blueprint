-- @+leo-ver=4-thin
-- @+node:gcross.20091127142612.1383:@thin Error.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091127142612.1398:<< Language extensions >>
{-# LANGUAGE TypeSynonymInstances #-}
-- @-node:gcross.20091127142612.1398:<< Language extensions >>
-- @nl

module Blueprint.Error where

-- @<< Import needed modules >>
-- @+node:gcross.20091127142612.1384:<< Import needed modules >>
import Control.Arrow
import Control.Applicative hiding (empty)
import Control.Applicative.Infix
import Control.Monad
import Control.Monad.Error

import Data.Either
import Data.Either.Unwrap
import Data.Monoid

import StringTable.Atom
import StringTable.AtomMap (AtomMap)
import qualified StringTable.AtomMap as Map

import Text.PrettyPrint.ANSI.Leijen
-- @-node:gcross.20091127142612.1384:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091127142612.1391:Types
-- @+node:gcross.20091127142612.1393:ErrorMessage
type ErrorMessage = AtomMap Doc
-- @-node:gcross.20091127142612.1393:ErrorMessage
-- @-node:gcross.20091127142612.1391:Types
-- @+node:gcross.20091127142612.1385:Instances
-- @+node:gcross.20091127142612.1386:Applicative (Either e a)
instance (Monoid e) => Applicative (Either e) where
    pure = Right
    (<*>) (Left error2) (Left error1) = Left (error1 `mappend` error2)
    (<*>) (Left error) _ = Left error
    (<*>) _ (Left error) = Left error
    (<*>) (Right function) (Right argument) = Right (function argument)
-- @-node:gcross.20091127142612.1386:Applicative (Either e a)
-- @+node:gcross.20091127142612.1388:Applicative (ErrorT e m a)
instance (Monoid e, Error e, Monad m) => Applicative (ErrorT e m) where
    pure = return
    e_fn <*> e_arg = ErrorT $ liftM2 (<*>) (runErrorT e_fn) (runErrorT e_arg)
-- @-node:gcross.20091127142612.1388:Applicative (ErrorT e m a)
-- @+node:gcross.20091128000856.1416:Error (ErrorMessage)
instance Error ErrorMessage where
    noMsg = strMsg "(and he did not even bother to include an error message!  :-/)"
    strMsg = errorMessage "caused by the programmer" . text
-- @-node:gcross.20091128000856.1416:Error (ErrorMessage)
-- @+node:gcross.20091129000542.1482:Monoid Doc
instance Monoid Doc where
    mempty = empty
    mappend = (<$$>)
    mconcat = vcat
-- @-node:gcross.20091129000542.1482:Monoid Doc
-- @+node:gcross.20091129000542.1602:Error Doc
instance Error Doc where
    noMsg = empty
    strMsg = text
-- @-node:gcross.20091129000542.1602:Error Doc
-- @-node:gcross.20091127142612.1385:Instances
-- @+node:gcross.20091127142612.1396:Functions
-- @+node:gcross.20091127142612.1397:extractResultsOrError
extractResultsOrError :: Monoid e => [Either e a] -> Either e [a]
extractResultsOrError = mapLeft mconcat . extractResultsOrErrors
-- @-node:gcross.20091127142612.1397:extractResultsOrError
-- @+node:gcross.20091127142612.1400:extractResultsOrErrors
extractResultsOrErrors :: [Either e a] -> Either [e] [a]
extractResultsOrErrors eithers =
    case partitionEithers (eithers) of
        ([],results) -> Right results
        (errors,_) -> Left errors
-- @-node:gcross.20091127142612.1400:extractResultsOrErrors
-- @+node:gcross.20091128000856.1425:formatErrorMessage
formatErrorMessage :: ErrorMessage -> Doc
formatErrorMessage = vcat . map (formatMessageWithHeading . first fromAtom) . Map.assocs
-- @-node:gcross.20091128000856.1425:formatErrorMessage
-- @+node:gcross.20091128000856.1427:formatMessageWithHeading
formatMessageWithHeading :: (String,Doc) -> Doc
formatMessageWithHeading (heading,message) =
    text ("Error " ++ heading ++ ":")
    <$$>
    indent 4 message
-- @-node:gcross.20091128000856.1427:formatMessageWithHeading
-- @+node:gcross.20091128000856.1428:errorMessage / leftErrorMessage
errorMessage :: String -> Doc -> ErrorMessage
errorMessage heading message = Map.singleton (toAtom heading) message

leftErrorMessage :: String -> Doc -> Either ErrorMessage a
leftErrorMessage heading = Left . errorMessage heading
-- @-node:gcross.20091128000856.1428:errorMessage / leftErrorMessage
-- @+node:gcross.20091128000856.1429:errorMessageText / leftErrorMessageText
errorMessageText :: String -> String -> ErrorMessage
errorMessageText heading = errorMessage heading . text

leftErrorMessageText :: String -> String -> Either ErrorMessage a
leftErrorMessageText heading = Left . errorMessageText heading
-- @-node:gcross.20091128000856.1429:errorMessageText / leftErrorMessageText
-- @+node:gcross.20091128000856.1431:errorMessageTextWithLines / leftErrorMessageTextWithLines
errorMessageTextWithLines :: String -> String -> ErrorMessage
errorMessageTextWithLines heading = errorMessage heading . vcat . map text . lines

leftErrorMessageTextWithLines :: String -> String -> Either ErrorMessage a
leftErrorMessageTextWithLines heading = Left . errorMessageTextWithLines heading
-- @-node:gcross.20091128000856.1431:errorMessageTextWithLines / leftErrorMessageTextWithLines
-- @-node:gcross.20091127142612.1396:Functions
-- @-others
-- @-node:gcross.20091127142612.1383:@thin Error.hs
-- @-leo
