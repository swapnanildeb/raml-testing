{-# LANGUAGE DeriveDataTypeable
           , EmptyDataDecls
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , TypeFamilies
           , UndecidableInstances
           , GADTs
 #-}


module Handler.Transaction where

import Import
import Handler.Common 

import Data.Aeson
import qualified Database.Esqueleto as E
import Data.List
import qualified Data.Map as Map
import qualified Prelude as P
import qualified Data.Text as T 

import Blockchain.Data.Transaction

import Handler.Filters

postTransactionR :: Handler ()
postTransactionR = do
   addHeader "Access-Control-Allow-Origin" "*"
   addHeader "Access-Control-Allow-Headers" "Content-Type"
  
   tx <- parseJsonBody :: Handler (Result RawTransaction')
   case tx of
       (Success (RawTransaction' raw "")) -> do
          let tx' = rawTX2TX raw
              h = toJSON $ transactionHash tx'
          insertTXIfNew Nothing [tx']
          case h of
            (String h') -> do
              sendResponseStatus status200 (h' :: Text)
            _ -> invalidArgs ["invalid transaction hash"]   
       _ -> invalidArgs ["couldn't decode transaction"]

optionsTransactionR :: Handler RepPlain
optionsTransactionR = do
  addHeader "Access-Control-Allow-Origin" "*"
  addHeader "Access-Control-Allow-Headers" "Content-Type"
  addHeader "Access-Control-Allow-Methods" "POST, OPTIONS"

  return $ RepPlain $ toContent ("" :: Text)

getTransactionR :: Handler Value
getTransactionR = do
                 getParameters <- reqGetParams <$> getRequest
                 appNameMaybe <- lookupGetParam "appname"
                 case appNameMaybe of
                   (Just t) -> liftIO $ putStrLn $ t
                   (Nothing) -> liftIO $ putStrLn "anon"

                 sortParam <- lookupGetParam "sortby"

--                 let offset = (fromIntegral $ (maybe 0 id $ extractPage "page" getParameters)  :: Int64)
                 let index' = (fromIntegral $ (maybe 0 id $ extractPage "index" getParameters)  :: Int)
                 let raw    = (fromIntegral $ (maybe 0 id $ extractPage "raw" getParameters) :: Integer) > 0
                 let paramMap = Map.fromList getParameters
                     paramMapRemoved = P.foldr (\param mp -> (Map.delete param mp)) paramMap transactionQueryParams
               
                 addHeader "Access-Control-Allow-Origin" "*"
                 txs <- case ((paramMapRemoved == Map.empty) && (paramMap /= Map.empty)) of 
                           False -> invalidArgs [T.concat ["Need one of: ", T.intercalate " , " $ transactionQueryParams]]
                           True ->  runDB $ E.select $
                                        E.from $ \(rawTx) -> do
                        
                                        E.where_ ((P.foldl1 (E.&&.) $ P.map (getTransFilter (rawTx)) $ getParameters ))

                                        let criteria = P.map (getTransFilter rawTx) $ getParameters 
                                        let allCriteria = ((rawTx E.^. RawTransactionBlockNumber) E.>=. E.val index') : criteria

                                        -- FIXME: if more than `limit` transactions per block, we will need to have a tuple as index
                                        E.where_ (P.foldl1 (E.&&.) allCriteria)

                                        -- E.offset $ (limit * offset)
                                        E.limit $ (limit)
                                        E.orderBy $ [(sortToOrderBy sortParam) $ (rawTx E.^. RawTransactionBlockNumber), 
                                                     (sortToOrderBy sortParam) $ (rawTx E.^. RawTransactionNonce)]

                                        return rawTx
                 --liftIO $ traceIO $ "number of results: " P.++ (show $ P.length txs)

                 let modTxs = (nub (P.map entityVal (txs :: [Entity RawTransaction])))
                 let newindex = pack $ show $ 1+(getTxNum $ P.last modTxs)
                 let extra p = P.zipWith extraFilter p (P.repeat (newindex))
                 -- this should actually use URL encoding code from Yesod
                 let next p = "/eth/v1.2/transaction?" P.++  (P.foldl1 (\a b -> (unpack a) P.++ "&" P.++ (unpack b)) $ P.map (\(k,v) -> (unpack k) P.++ "=" P.++ (unpack v)) (extra p))
                 let addedParam = appendIndex getParameters

                 toRet raw modTxs (next addedParam) 
               where
                   toRet raw bs gp = case if' raw bs (P.map rtToRtPrime (P.zip (P.repeat gp) bs)) of 
                              Left a -> returnJson a
                              Right b -> returnJson b -- bandaid
                   limit = (fromIntegral $ fetchLimit :: Int64)


                   
                   
