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

module Handler.AccountInfo where

import Import

import Handler.Common 
import Handler.Filters

import qualified Database.Esqueleto as E
import Data.List
import qualified Data.Map as Map

import qualified Prelude as P
import qualified Data.Text as T


getAccountInfoR :: Handler Value
getAccountInfoR = do
                 getParameters <- reqGetParams <$> getRequest
                 
                 appNameMaybe <- lookupGetParam "appname"
                 case appNameMaybe of
                     (Just t) -> liftIO $ putStrLn $ t
                     (Nothing) -> liftIO $ putStrLn "anon"


--                 let offset = fromIntegral $ (maybe 0 id $ extractPage "page" getParameters)  :: Int64
                 let index' = fromIntegral (maybe 0 id $  extractPage "index" getParameters)  :: Int64
--                 let raw    = (fromIntegral $ (maybe 0 id $ extractPage "raw" getParameters) :: Integer) > 0


                 let paramMap = Map.fromList getParameters
                     paramMapRemoved = P.foldr (\param mp -> (Map.delete param mp)) paramMap accountQueryParams

                 addHeader "Access-Control-Allow-Origin" "*"
                 addrs <- case ((paramMapRemoved == Map.empty) && (paramMap /= Map.empty)) of
                           False -> invalidArgs [T.concat ["Need one of: ", T.intercalate " , " $ accountQueryParams]]
                           True ->  runDB $ E.select . E.distinct $

                                        E.from $ \(accStateRef) -> do

                                        let criteria = P.map (getAccFilter (accStateRef)) $ getParameters 
                                        let allCriteria = ((accStateRef E.^. AddressStateRefId) E.>=. E.val (E.toSqlKey index')) : criteria

                                        E.where_ (P.foldl1 (E.&&.) allCriteria)

                                        -- E.offset $ (limit * offset)
                                        E.limit $ limit

                                        E.orderBy [E.asc (accStateRef E.^. AddressStateRefId)]


                                        return accStateRef

                 returnJson $ nub $ P.map asrToAsrPrime (P.map entityVal (addrs :: [Entity AddressStateRef])) 
             where
                 limit = (fromIntegral $ fetchLimit :: Int64)
