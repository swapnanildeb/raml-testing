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

module Handler.StorageInfo where

import Import
import Handler.Common 
import Handler.Filters

import Data.List

import qualified Database.Esqueleto as E

import qualified Prelude as P


getStorageInfoR :: Handler Value
getStorageInfoR = do
                 getParameters <- reqGetParams <$> getRequest
                 
                 appNameMaybe <- lookupGetParam "appname"
                 case appNameMaybe of
                     (Just t) -> liftIO $ putStrLn $ t
                     (Nothing) -> liftIO $ putStrLn "anon"


  --               let offset = (fromIntegral $ (maybe 0 id $ extractPage "page" getParameters)  :: Int64)
 --                let index' = (fromIntegral $ (maybe 0 id $ extractPage "index" getParameters)  :: Integer)

                 addHeader "Access-Control-Allow-Origin" "*"

                 addrs <- runDB $ E.select . E.distinct $

                                        E.from $ \(storage `E.InnerJoin` addrStRef) -> do
                        
                                        E.on ( storage E.^. StorageAddressStateRefId E.==. addrStRef E.^. AddressStateRefId )                                        

                                        E.where_ ((P.foldl1 (E.&&.) $ P.map (getStorageFilter (storage,addrStRef)) $ getParameters ))

--                                        E.offset $ (limit * offset)
                                        E.limit $ limit

                                        E.orderBy [E.asc (storage E.^. StorageKey)]

                                        return storage
                 --liftIO $ traceIO $ "number of results: " P.++ (show $ P.length addrs)
                 returnJson $ nub $ P.map entityVal (addrs :: [Entity Storage]) -- consider removing nub - it takes time n^{2}
                 where 
                   limit = (fromIntegral $ fetchLimit :: Int64)
                  
