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

module Handler.LogInfo where

import Import

import Handler.Common 
import Handler.Filters

import qualified Database.Esqueleto as E
import Data.List

import qualified Prelude as P



getLogInfoR :: Handler Value
getLogInfoR = do
                 getParameters <- reqGetParams <$> getRequest
                 
                 let index' = fromIntegral (maybe 0 id $  extractPage "index" getParameters)  :: Int64

                 addHeader "Access-Control-Allow-Origin" "*"

                 logs <- runDB $ E.select $

                                        E.from $ \(lg) -> do

                                        let criteria = P.map (getLogFilter lg) $ getParameters 
                                        let allCriteria = ((lg E.^. LogDBId) E.>=. E.val (E.toSqlKey index')) : criteria

                                        E.where_ (P.foldl1 (E.&&.) allCriteria)

                                        E.limit $ limit

                                        E.orderBy [E.desc (lg E.^. LogDBId)]

                                        return lg

                 returnJson $ nub $ P.map entityVal (logs :: [Entity LogDB])
             where
                 limit = (fromIntegral $ fetchLimit :: Int64)
