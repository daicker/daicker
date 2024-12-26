module Language.Daicker.StdLib.HTTP where

import Control.Comonad.Cofree (Cofree ((:<)))
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromJust)
import Language.Daicker.AST
import Language.Daicker.Span (Span (FixtureSpan))
import Network.HTTP.Client
  ( Response (responseBody),
    httpLbs,
    newManager,
    parseRequest,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)

eString :: String -> Expr Span
eString s = httpSpan :< EString s

sGet :: Statement Span
sGet =
  httpSpan
    :< SExpr
      (httpSpan :< Identifier "get")
      ( httpSpan
          :< EFixtureFun
            [ httpSpan
                :< PositionedParameter
                  (httpSpan :< Identifier "url")
                  False
                  False
                  (Just $ httpSpan :< TVar (httpSpan :< Identifier "String"))
                  Nothing
            ]
            ( \sp args -> do
                let (_ :< EString url) = fromJust $ lookup "url" args
                manager <- newManager tlsManagerSettings
                request <- parseRequest url
                response <- httpLbs request manager
                let body = responseBody response
                pure $ eString (B.unpack body)
            )
            (Just $ httpSpan :< TVar (httpSpan :< Identifier "String"))
      )

mHttp :: Module Span
mHttp =
  httpSpan
    :< Module
      []
      Nothing
      [sGet]

httpSpan :: Span
httpSpan = FixtureSpan "http"
