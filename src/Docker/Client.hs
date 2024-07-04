{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Docker.Client where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import Control.Monad.Reader.Class
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS

-- newtype HttpHandler m = HttpHandler (forall a. Request -> (HTTP.Response () -> Sink BSC.ByteString m (Either DockerError a)) -> m (Either DockerError a))

-- main :: IO ()
-- main = do
--   withSocketsDo $ do
--     soc <- socket AF_UNIX Stream 0
--     connect (soc) (SockAddrUnix "/var/run/docker.sock")
--     send soc ("test123")
--     close soc

-- defaultUnixManagerSettings ::
--   FilePath ->
--   HTTP.ManagerSettings
-- defaultUnixManagerSettings fp =
--   defaultManagerSettings
--     { HTTP.managerRawConnection = return $ openUnixSocket fp
--     }
--   where
--     openUnixSocket filePath _ _ _ = do
--       s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
--       S.connect s (S.SockAddrUnix filePath)
--       makeConnection
--         (SBS.recv s 8096)
--         (SBS.sendAll s)
--         (S.close s)

-- unixHttpHandler ::
--   ( MonadUnliftIO m,
--     MonadIO m,
--     MonadMask m
--   ) =>
--   FilePath ->
--   m (HttpHandler m)
-- unixHttpHandler fp = do
--   let mSettings = defaultUnixManagerSettings fp
--   manager <- liftIO $ newManager mSettings
--   return $ httpHandler manager
