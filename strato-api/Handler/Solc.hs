{-# LANGUAGE OverloadedStrings, ExplicitForAll, Rank2Types, ScopedTypeVariables #-}

module Handler.Solc (postSolcR) where

import Control.Monad
import Control.Monad.Trans.Either
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Map ()
import qualified Data.Text as Text
import qualified Data.Traversable as Trv

import Import
import Handler.SolidityCommon

import System.Directory
import System.Exit
import System.IO as IO
import System.IO.Temp
import System.Process

-- Query parameters allowed:
--   src: solidity source code to be compiled, as a (url-encoded) string
--   optimize, add-std, link: flags for "solc" executable
--   optimize-runs, libraries: options with arguments for "solc" executable
-- Data allowed:
--   main: a Solidity source file to be compiled
--   import: a Solidity source file that is included by another one
-- Response:
--   { <contract name> : { abi : <solidity contract abi>, bin : <hex string> } }

postSolcR :: Handler Text
postSolcR = do
  addHeader "Access-Control-Allow-Origin" "*"
  (postParams, mainFiles, importFiles) <- getSolSrc
  eitherErrEncode $ runSolc postParams mainFiles importFiles
  
runSolc :: Map String String -> Map String String -> Map String String
           -> EitherT String IO (Map String Aeson.Value)
runSolc optsObj mainSrc importsSrc =
  execSolc solcCompileOpts solcLinkOpts mainSrc importsSrc
  where
    solcCompileOpts = concat [
      solcOParam, solcORunsParam, solcStdParam, ["--combined-json=abi,bin"]
      ]
    solcLinkOpts = concat [solcLinkParam, solcLibsParam]

    solcOParam = optNoArg "optimize" optsObj
    solcORunsParam = optWithArg "optimize-runs" optsObj
    solcStdParam = optNoArg "add-std" optsObj
    
    solcLinkParam = optNoArg "link" optsObj
    solcLibsParam = optWithArg "libraries" optsObj
                    
execSolc :: [String] -> [String] -> Map String String -> Map String String
            -> EitherT String IO (Map String Aeson.Value)
execSolc compileOpts linkOpts mainSrc importsSrc =
  doWithEitherT inTempDir $ do
    makeSrcFiles importsSrc
    compiledFiles <- Trv.forM mainSrc $ solcFile compileOpts linkOpts
    return $ Map.filter
      (\file -> case file of
          Aeson.Null -> False
          _ -> True)
      compiledFiles

solcFile :: [String] -> [String] -> String -> EitherT String IO Aeson.Value
solcFile compileOpts linkOpts src = do
  solcOutput <- callSolc compileOpts src
  solcJSON0 <- hoistEither $ aesonDecodeUtf8 $ Text.pack solcOutput
  let solcJSON1 = Map.lookup ("contracts" :: String) solcJSON0 

  let nullOutput n = liftM $ either (maybe (Right n) Left) Right
  mapEitherT (nullOutput $ Aeson.Null) $ do
    solcJSON :: Map String (Map String Aeson.Value) <-
      case Aeson.fromJSON <$> solcJSON1 of
        Just (Aeson.Error err) -> left $ Just err
        Just (Aeson.Success m) -> right m
        Nothing -> left Nothing

    let linkBin binabi bin =
          if (not . null $ linkOpts)
          then do
            linkedBin <- bimapEitherT Just id $ callSolc linkOpts bin
            return $ Map.insert "bin" (Aeson.toJSON linkedBin) binabi
          else return binabi

    linkJSON <- Trv.forM solcJSON $ \binabi ->
      bimapEitherT Just id $ mapEitherT (nullOutput Map.empty) $ do
        bin <- case Aeson.fromJSON <$> Map.lookup "bin" binabi of
          Just (Aeson.Error err) -> left $ Just err
          Just (Aeson.Success "") -> left Nothing
          Just (Aeson.Success s) -> right s
          Nothing -> left Nothing
        linkBin binabi bin
    
    return $ Aeson.toJSON $ Map.filter (not . Map.null) linkJSON

callSolc :: [String] -> String -> EitherT String IO String
callSolc opts input =
  execWithEitherT $ readProcessWithExitCode "solc" opts input

makeSrcFiles :: Map String String -> EitherT String IO ()
makeSrcFiles importsSrc =
  liftIO $
  (Map.foldrWithKey (\k s x -> IO.writeFile k s >> x) (return ())) importsSrc

doWithEitherT :: (forall b.(IO b -> IO b)) -> (EitherT c IO d -> EitherT c IO d)
doWithEitherT runner cmd = do
  let cmdIO = runEitherT cmd
  resultE <- liftIO $ runner cmdIO
  hoistEither resultE

execWithEitherT :: IO (ExitCode, String, String) -> EitherT String IO String
execWithEitherT op = do
  (exitCode, stdOut, stdErr) <- liftIO op
  case exitCode of
    ExitSuccess -> right stdOut
    _ -> left stdErr

inTempDir :: IO a -> IO a
inTempDir act =
  withSystemTempDirectory "solc" $ \dir -> withCurrentDirectory dir act

optNoArg :: String -> Map String String -> [String]
optNoArg opt opts =
  maybe [] (const ["--" ++ opt]) $ Map.lookup opt opts

optWithArg :: String -> Map String String -> [String]
optWithArg opt opts =
  maybe [] (\arg -> ["--" ++ opt, arg]) $ Map.lookup opt opts
