module Handler.Filters where

import qualified Database.Esqueleto as E

import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Binary as Bin
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Base16 as B16
import Database.Persist
import Database.Persist.Postgresql
import Numeric

import Blockchain.SHA
import Blockchain.ExtWord
import Blockchain.Util
import Blockchain.Data.DataDefs
import Blockchain.Data.Address

import Control.Monad
import Data.Set

import Import

sortToOrderBy :: (E.Esqueleto query expr backend, PersistField a) 
            => Maybe Text -> expr (E.Value a) -> (expr E.OrderBy)
sortToOrderBy (Just "asc")  x  = E.asc  x
sortToOrderBy (Just "desc") x  = E.desc x
sortToOrderBy _             x  = E.asc  x

blockQueryParams:: [Text]
blockQueryParams = [ "txaddress",
                     "coinbase",
                     "address",
                     "blockid",
                     "hash",
                     "mindiff",
                     "maxdiff",
                     "diff",
                     "gasused",
                     "mingasused",
                     "maxgasused",
                     "gaslim",
                     "mingaslim",
                     "maxgaslim",
                     "number",
                     "minnumber", 
                     "maxnumber" ]

-- todo: eliminate the Entity Block from this function
getBlkFilter :: (E.Esqueleto query expr backend) => (expr (Entity BlockDataRef), expr (Entity AddressStateRef), expr (Entity RawTransaction), expr (Entity Block))-> (Text, Text) -> expr (E.Value Bool)

getBlkFilter  _                               ("page", _)    = E.val True 
getBlkFilter  _                               ("index", _)    = E.val True 
getBlkFilter  _                               ("raw", _)    = E.val True 
getBlkFilter  _                               ("next", _)    = E.val True
getBlkFilter  _                               ("prev", _)    = E.val True
getBlkFilter  _                               ("appname", _) = E.val True
getBlkFilter (bdRef, _, _, _)                 ("ntx", v)    = bdRef E.^. BlockDataRefNumber E.==. E.val (P.read $ T.unpack v :: Integer)

getBlkFilter (bdRef, _, _, _)                 ("number", v)    = bdRef E.^. BlockDataRefNumber E.==. E.val (P.read $ T.unpack v :: Integer)
getBlkFilter (bdRef, _, _, _)                 ("minnumber", v)    = bdRef E.^. BlockDataRefNumber E.>=. E.val (P.read $ T.unpack v :: Integer)
getBlkFilter (bdRef, _, _, _)                 ("maxnumber", v)    = bdRef E.^. BlockDataRefNumber E.<=. E.val (P.read $ T.unpack v :: Integer)

getBlkFilter (bdRef, _, _, _)                 ("gaslim", v)    = bdRef E.^. BlockDataRefGasLimit E.==. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("mingaslim", v) = bdRef E.^. BlockDataRefGasLimit E.>=. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("maxgaslim", v) = bdRef E.^. BlockDataRefGasLimit E.<=. E.val (P.read $ T.unpack v :: Integer) 

getBlkFilter (bdRef, _, _, _)                 ("gasused", v)    = bdRef E.^. BlockDataRefGasUsed E.==. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("mingasused", v) = bdRef E.^. BlockDataRefGasUsed E.>=. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("maxgasused", v) = bdRef E.^. BlockDataRefGasUsed E.<=. E.val (P.read $ T.unpack v :: Integer) 

getBlkFilter (bdRef, _, _, _)                 ("diff", v)      = bdRef E.^. BlockDataRefDifficulty E.==. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("mindiff", v)   = bdRef E.^. BlockDataRefDifficulty E.>=. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("maxdiff", v)   = bdRef E.^. BlockDataRefDifficulty E.<=. E.val (P.read $ T.unpack v :: Integer) 

-- getBlkFilter (bdRef, accStateRef, rawTX, blk) ("time", v)      = bdRef E.^. BlockDataRefTimestamp E.==. E.val (stringToDate v)
-- getBlkFilter (bdRef, accStateRef, rawTX, blk) ("mintime", v)   = bdRef E.^. BlockDataRefTimestamp E.>=. E.val (stringToDate v)
-- getBlkFilter (bdRef, accStateRef, rawTX, blk) ("maxtime", v)   = bdRef E.^. BlockDataRefTimestamp E.<=. E.val (stringToDate v)

getBlkFilter (_, _, rawTX, _) ("txaddress", v) = (rawTX E.^. RawTransactionFromAddress E.==. E.val (toAddr v))
                                                 E.||. (rawTX E.^. RawTransactionToAddress E.==. E.val (Just (toAddr v)))

getBlkFilter (bdRef, _, _, blk) ("coinbase", v)  = bdRef E.^. BlockDataRefCoinbase E.==. E.val (toAddr v)
                                                              E.&&. ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId)
getBlkFilter (_, accStateRef, _, _) ("address", v)   = accStateRef E.^. AddressStateRefAddress E.==. E.val (toAddr v)
getBlkFilter (bdRef, _, _, blk) ("blockid", v)   = bdRef E.^. BlockDataRefBlockId E.==. E.val (toBlockId v)
                                                              E.&&. ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId)
getBlkFilter (bdRef, _, _, blk) ("hash", v)   = (bdRef E.^. BlockDataRefHash E.==. E.val ( SHA . fromIntegral . byteString2Integer . fst. B16.decode $ T.encodeUtf8 $ v ) ) E.&&. ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId)
                            

getBlkFilter _ _ = P.undefined ("no match in getBlkFilter"::String)



accountQueryParams:: [Text]
accountQueryParams = [ "address",
                       "balance",
                       "minbalance",
                       "maxbalance",
                       "nonce",
                       "minnonce",
                       "maxnonce", 
                       "maxnumber" ]


getAccFilter :: (E.Esqueleto query expr backend) => (expr (Entity AddressStateRef))-> (Text, Text) -> expr (E.Value Bool)
getAccFilter  _            ("page", _)         =  E.val True
getAccFilter  _            ("index", _)        =  E.val True
getAccFilter  _            ("raw", _)          =  E.val True
getAccFilter  _            ("next", _)         =  E.val True
getAccFilter  _            ("prev", _)         =  E.val True
getAccFilter  _            ("appname", _)      =  E.val True

getAccFilter (accStateRef) ("balance", v)      = accStateRef E.^. AddressStateRefBalance E.==. E.val (P.read $ T.unpack v :: Integer) 
getAccFilter (accStateRef) ("minbalance", v)   = accStateRef E.^. AddressStateRefBalance E.>=. E.val (P.read $ T.unpack v :: Integer) 
getAccFilter (accStateRef) ("maxbalance", v)   = accStateRef E.^. AddressStateRefBalance E.<=. E.val (P.read $ T.unpack v :: Integer) 

getAccFilter (accStateRef) ("nonce", v)        = accStateRef E.^. AddressStateRefNonce E.==. E.val (P.read $ T.unpack v :: Integer)
getAccFilter (accStateRef) ("minnonce", v)     = accStateRef E.^. AddressStateRefNonce E.>=. E.val (P.read $ T.unpack v :: Integer)
getAccFilter (accStateRef) ("maxnonce", v)     = accStateRef E.^. AddressStateRefNonce E.<=. E.val (P.read $ T.unpack v :: Integer)

getAccFilter (accStateRef) ("address", v)      = accStateRef E.^. AddressStateRefAddress E.==. E.val (toAddr v)
getAccFilter _             _                   = P.undefined ("no match in getAccFilter"::String)

transactionQueryParams:: [Text]
transactionQueryParams = [ "address",
                           "from",
                           "to",
                           "hash",
                           "gasprice",
                           "mingasprice",
                           "maxgasprice",
                           "gaslimit",
                           "mingaslimit",
                           "maxgaslimit",
                           "value",
                           "minvalue", 
                           "maxvalue",
                           "blocknumber" ]

getTransFilter :: (E.Esqueleto query expr backend) => (expr (Entity RawTransaction))-> (Text, Text) -> expr (E.Value Bool)
getTransFilter  _          ("page", _)         = E.val True
getTransFilter  _          ("index", _)        = E.val True
getTransFilter  _          ("raw", _)          = E.val True
getTransFilter  _          ("next", _)         = E.val True
getTransFilter  _          ("prev", _)         = E.val True
getTransFilter  _          ("appname", _)      = E.val True
getTransFilter  _          ("sortby", _)       = E.val True

getTransFilter (rawTx)     ("address", v)      = rawTx E.^. RawTransactionFromAddress E.==. E.val (toAddr v) E.||. rawTx E.^. RawTransactionToAddress E.==. E.val (Just (toAddr v))
getTransFilter (rawTx)     ("from", v)         = rawTx E.^. RawTransactionFromAddress E.==. E.val (toAddr v)
getTransFilter (rawTx)     ("to", v)           = rawTx E.^. RawTransactionToAddress E.==. E.val (Just (toAddr v))
getTransFilter (rawTx)     ("hash", v)         = rawTx E.^. RawTransactionTxHash  E.==. E.val ( SHA . fromIntegral . byteString2Integer . fst. B16.decode $ T.encodeUtf8 $ v ) 

--getTransFilter (rawTx)     ("type", "Contract") = (rawTx E.^. RawTransactionToAddress E.==. (E.val "")) E.&&. (RawTransactionCodeOrData E.!=. (E.val ""))

getTransFilter (rawTx)     ("gasprice", v)     = rawTx E.^. RawTransactionGasPrice E.==. E.val (P.read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("mingasprice", v)  = rawTx E.^. RawTransactionGasPrice E.>=. E.val (P.read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("maxgasprice", v)  = rawTx E.^. RawTransactionGasPrice E.<=. E.val (P.read $ T.unpack v :: Integer)

getTransFilter (rawTx)     ("gaslimit", v)     = rawTx E.^. RawTransactionGasLimit E.==. E.val (P.read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("mingaslimit", v)  = rawTx E.^. RawTransactionGasLimit E.>=. E.val (P.read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("maxgaslimit", v)  = rawTx E.^. RawTransactionGasLimit E.<=. E.val (P.read $ T.unpack v :: Integer)

getTransFilter (rawTx)     ("value", v)        = rawTx E.^. RawTransactionValue E.==. E.val (P.read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("minvalue", v)     = rawTx E.^. RawTransactionValue E.>=. E.val (P.read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("maxvalue", v)     = rawTx E.^. RawTransactionValue E.<=. E.val (P.read $ T.unpack v :: Integer)

getTransFilter (rawTx)     ("blocknumber", v)  = rawTx E.^. RawTransactionBlockNumber E.==. E.val (P.read $ T.unpack v :: Int)
getTransFilter _           _                   = P.undefined ("no match in getTransFilter"::String)

getStorageFilter :: (E.Esqueleto query expr backend) => (expr (Entity Storage), expr (Entity AddressStateRef)) -> (Text, Text) -> expr (E.Value Bool)
getStorageFilter _ ("page",_)  = E.val True
getStorageFilter _ ("index",_) = E.val True
getStorageFilter (storage,_) ("key", v)
  = storage E.^. StorageKey E.==. E.val (P.fromIntegral (P.read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("minkey", v)
  = storage E.^. StorageKey E.>=. E.val (P.fromIntegral (P.read $ T.unpack v :: Integer) :: Word256) 
getStorageFilter (storage,_) ("maxkey", v)
  = storage E.^. StorageKey E.<=. E.val (P.fromIntegral (P.read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("keystring", v)
  = storage E.^. StorageKey E.==. E.val (Bin.decode $ BS.fromStrict $ T.encodeUtf8 v :: Word256)
getStorageFilter (storage,_) ("keyhex", v)
  = storage E.^. StorageKey E.==. E.val (fromHexText v)
getStorageFilter (storage,_) ("value", v)
  = storage E.^. StorageValue E.==. E.val (P.fromIntegral (P.read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("minvalue", v)
  = storage E.^. StorageValue E.>=. E.val (P.fromIntegral (P.read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("maxvalue", v)
  = storage E.^. StorageValue E.<=. E.val (P.fromIntegral (P.read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("valuestring", v)
  = storage E.^. StorageValue E.==. E.val (Bin.decode $ BS.fromStrict $ T.encodeUtf8 v :: Word256)
getStorageFilter (storage,_) ("addressid", v)
  = storage E.^. StorageAddressStateRefId E.==. E.val (toAddrId v)
getStorageFilter (_,addrStRef) ("address", v)      -- Note: a join is done in StorageInfo
  = addrStRef E.^. AddressStateRefAddress E.==. E.val (toAddr v)  
getStorageFilter _           _                   = P.undefined ("no match in getStorageFilter"::String)

getLogFilter :: (E.Esqueleto query expr backend) => expr (Entity LogDB) -> (Text, Text) -> expr (E.Value Bool)
getLogFilter _ ("index",_) = E.val True         -- indexes are intercepted in handlers. We should probably deal with them here in the future
getLogFilter log' ("address",v) = log' E.^. LogDBAddress E.==. E.val (toAddr v)  
getLogFilter log' ("hash",v) = log' E.^. LogDBTransactionHash  E.==. E.val ( SHA . fromIntegral . byteString2Integer . fst. B16.decode $ T.encodeUtf8 $ v ) 
getLogFilter _           _  = P.undefined ("no match in getLogFilter"::String)

toAddrId :: Text -> Key AddressStateRef
toAddrId = toId

toBlockId :: Text -> Key Block
toBlockId = toId

toId :: ToBackendKey SqlBackend record => Text -> Key record
toId v = toSqlKey (fromIntegral $ (P.read $ T.unpack v :: Integer) )

toAddr :: Text -> Address
toAddr v = Address wd160
  where ((wd160, _):_) = readHex $ T.unpack $ v :: [(Word160,String)]


extractValue :: String -> [(Text, Text)] -> String -> Maybe String
extractValue name ts zero = Control.Monad.foldM toFold zero (P.map selectPage ts)
     where 
       toFold :: String -> Maybe String -> Maybe String
       toFold n Nothing = Just n
       toFold n (Just m) = Just (P.maximum [n, m])
       selectPage :: (Text, Text) -> Maybe String
       selectPage (s, v) | T.unpack s == name = Just $ T.unpack v
                         | otherwise = Nothing

fromHexText :: T.Text -> Word256
fromHexText v = res 
  where ((res,_):_) = readHex $ T.unpack $ v :: [(Word256,String)]

extractPage :: String -> [(Text, Text)] ->  Maybe Integer 
extractPage name ts = Control.Monad.foldM toFold 0 (P.map selectPage ts)
     where 
       toFold :: Integer -> Maybe Integer -> Maybe Integer
       toFold n Nothing = Just n
       toFold n (Just m) = Just (P.maximum [n, m])
       selectPage :: (Text, Text) -> Maybe Integer
       selectPage (s, v) | T.unpack s == name = Just (P.read $ T.unpack v :: Integer)
                         | otherwise = Nothing

toParam :: (Text,Text) -> Param
toParam a = Param a

fromParam :: Param -> (Text,Text)
fromParam (Param a) = a

data Param = Param (Text,Text)
instance Eq Param where
  Param a == Param b = fst a == fst b
instance Ord Param where
  (Param a) `compare` (Param b) = (fst a) `compare` (fst b)

appendIndex :: [(Text, Text)] -> [(Text,Text)] -- this sould be using URL encoding code from Yesod
appendIndex l = P.map fromParam (Data.Set.toList $ Data.Set.insert (toParam ("index", "")) $ Data.Set.fromList $ P.map toParam l)

extraFilter :: (Text,Text) -> Text -> (Text,Text)
extraFilter ("index", _) v' = ("index", v')
extraFilter (a,b) _'        = (a,b)

getBlockNum :: Block -> Integer
getBlockNum (Block (BlockData _ _ (Address _) _ _ _ _ _ num _ _ _ _ _ _) _ _) = num 

getTxNum :: RawTransaction -> Int
getTxNum (RawTransaction _ (Address _) _ _ _ _ _ _ _ _ _ bn _ _) = bn

-- probably need to pad here
getAccNum :: AddressStateRef -> String
getAccNum (AddressStateRef (Address x) _ _ _ _ _) = (showHex x "")

if' :: Bool -> a -> b -> Either a b
if' x a b = if x == True then Left a else Right b
