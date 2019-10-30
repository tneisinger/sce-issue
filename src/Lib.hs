{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( startApp
    , app
    , api
    , server
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.HTTP.Types.Status (mkStatus)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Checked.Exceptions
  ( Throws
  , Envelope(..)
  , ErrStatus(..)
  , toSuccEnvelope
  , toErrEnvelope
  )

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data NoUsersError = NoUsersError
  deriving (Eq, Read, Show)

instance ErrStatus NoUsersError where
  toErrStatus _ = mkStatus 404 "NoUsersError"

$(deriveJSON defaultOptions ''NoUsersError)

type API = Throws NoUsersError :> "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = getUsers

getUsers :: Handler (Envelope '[NoUsersError] [User])
getUsers = if length users > 0
             then pure $ toSuccEnvelope users
             else pure $ toErrEnvelope NoUsersError

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
