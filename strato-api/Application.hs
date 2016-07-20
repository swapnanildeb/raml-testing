{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where


import Control.Monad.Logger      
import Database.Persist.Postgresql
import qualified Database.PostgreSQL.Simple as PG

import Import 
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Handler.WarpTLS          
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet)

import Handler.Common
import Handler.Home
import Handler.Transaction
import Handler.AccountInfo
import Handler.BlockInfo
import Handler.Demo
import Handler.Help
import Handler.TxLast
import Handler.BlkLast
import Handler.Test
import Handler.QueuedTransactions
import Handler.PushTransaction
import Handler.Solc
import Handler.AfterSubmission
import Handler.TransactionDemo
import Handler.StorageInfo
import Handler.TransactionResult
import Handler.Faucet
import Handler.ExtABI
import Handler.Wallet
import Handler.Login
import Handler.RegisterApp
import Handler.Developer
import Handler.Stats
import Handler.Tutorial
import Handler.ApiDocs
import Handler.Raml
import Handler.AppCreate
import Handler.Version
import Handler.Coinbase
import Handler.LogInfo

import Blockchain.EthConf

mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and return a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
--        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
--        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <-  myLogger $ myPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    --runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
    _ <- myLogger (runSqlPool (runMigrationSilent migrateAll) pool) --runMigration

    -- Return the foundation
    return $ mkFoundation pool

myLogger :: NoLoggingT m a -> m a
myLogger = runNoLoggingT --runStdoutLoggingT

noPool :: PG.Connection -> IO ()
noPool = const $ return ()

{-
prePool :: PG.Connection -> IO ()
prePool conn = withConnection conn $ const $ do
                            --id <- PQ.backendPID
                            liftIO $ traceIO $ "hello"
                            return ()
-}

myPool :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) 
       => ConnectionString -> Int -> m ConnectionPool
myPool = createPostgresqlPoolModified $ noPool


-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applyng some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiApp foundation
    return $ logWare $ defaultMiddlewaresNoLogging app

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings 

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadAppSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadAppSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

  {- CONFIG gradual change -}

    let oldDbSettings = appDatabaseConf settings
        settings' = settings { 
                      appDatabaseConf = oldDbSettings { 
                        pgConnStr = connStr
                    }
                  }
    -- Generate the foundation from the settings
    foundation <- makeFoundation settings'

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    -- runSettings (warpSettings foundation) app
    runTLS tls (warpSettings foundation) app
  where 
    tls = (tlsSettingsChain "certs/star_blockapps_net.pem" ["certs/TrustedRoot.pem", "certs/DigiCertCA2.pem"] "certs/key.pem"){onInsecure=AllowInsecure}


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB
