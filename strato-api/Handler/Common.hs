{-# LANGUAGE FlexibleInstances #-}

module Handler.Common 
  ( 
    module Handler.Common,
    module Blockchain.Data.DataDefs,
    module Blockchain.Data.Json
  )
where

import Data.FileEmbed (embedFile)
import Import
import Blockchain.Data.DataDefs
import Blockchain.Data.Json

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")             

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain $ toContent $(embedFile "config/robots.txt")

-- We use this to throttle queries
fetchLimit :: Int64
fetchLimit = 100
