{-# LANGUAGE OverloadedStrings #-}

module Handler.ExtABI (postExtABIR) where

import qualified Blockchain.Ethereum.Solidity.Parse as Solidity
import Blockchain.Ethereum.Solidity.External.JSON

import Control.Monad.Trans.Either
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import qualified Data.Text as Text

import Import
import Handler.SolidityCommon

-- Query parameters allowed:
--   src: solidity source code to be parsed, as a (url-encoded) string
-- Data allowed:
--   main: a Solidity source file to be parsed
--   import: a Solidity source file that is included by another one

postExtABIR :: Handler Text
postExtABIR = do
  addHeader "Access-Control-Allow-Origin" "*"
  (_, mainFiles, importFiles) <- getSolSrc
  eitherErrEncode $ allXABI importFiles mainFiles

allXABI :: Map String String -> Map String String -> EitherT String IO Aeson.Value
allXABI importFiles mainFiles = do
  results <- mapM (uncurry $ fileXABI importFiles) $ Map.toList mainFiles
  return $ Aeson.object results

fileXABI :: Map String String -> String -> String
            -> EitherT String IO (Text, Aeson.Value)
fileXABI importFiles name src = do
  result <- bimapEitherT show id $ hoistEither $
            jsonABI <$> Solidity.parse (importFiles Map.!) name src
  return (Text.pack name, result)
