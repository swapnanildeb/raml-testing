{-# LANGUAGE OverloadedStrings #-}

module Handler.Faucet where

import qualified Data.Text as T

import qualified Network.Haskoin.Crypto as H

import Import
import Handler.Filters
import Handler.Common

import qualified Data.Binary as BN

import qualified Database.Esqueleto as E
import Blockchain.Data.Address
import Blockchain.Constants
import Blockchain.Data.Transaction

import qualified Prelude as P

retrievePrvKey :: FilePath -> IO (Maybe H.PrvKey)
retrievePrvKey path = do
  keyBytes <- readFile path
  let intVal = BN.decode $ keyBytes :: Integer
  return $ H.makePrvKey intVal


lookupNonce :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) => Address -> HandlerT site IO Integer
lookupNonce addr' = do
  addrSt <- runDB $ E.select $ 
                      E.from $ \(accStateRef) -> do
                      E.where_ (accStateRef E.^. AddressStateRefAddress E.==. (E.val addr'))
                      return accStateRef
  case addrSt of 
    [] -> return 0
    addrSt' -> return $ addressStateRefNonce $ E.entityVal $ P.head $ addrSt'

postFaucetR :: Handler Text
postFaucetR = do
  addHeader "Access-Control-Allow-Origin" "*"

  key <- liftIO $ retrievePrvKey $ "config" </> "priv"
  key' <- maybe (invalidArgs ["No faucet account is defined"]) return key 
  liftIO $ putStrLn $ T.pack $ show key'

  nonce <- lookupNonce $ prvKey2Address key'

  maybeVal <- lookupPostParam "address"
  liftIO $ putStrLn $ T.pack $ show maybeVal
  case maybeVal of
    Just val -> putTX key' val nonce 
    Nothing -> do
      maybeAddrs <- lookupPostParam "addresses"
      liftIO $ putStrLn $ T.pack $ show maybeAddrs
      case maybeAddrs of
        Just addrTLT -> do
          let addrTL = P.read $ T.unpack $ addrTLT
          faucets <- sequence $ zipWith (putTX key') addrTL $ map (nonce +) [0..]
          return $ T.pack $ show $ map T.unpack faucets
        Nothing -> invalidArgs ["Missing 'address' or 'addresses'"]
  where
    makeTX k a n = do
      liftIO $ putStrLn $ T.pack $ "nonce: " ++ (show n)
      tx <- liftIO $ H.withSource H.devURandom (createMessageTX n (50000000000) (100000) a (1000*ether) "" k)
      liftIO $ putStrLn $ T.pack $ "tx for faucet: " ++ (show tx)
      return tx
    putTX k v n = do 
      tx <- makeTX k (toAddr v) n
      _ <- insertTXIfNew Nothing [tx]
      return $ "/account?address=" ++ v


