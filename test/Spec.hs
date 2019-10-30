{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main (main) where

import Data.Proxy (Proxy(..))
import Lib (api, server)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Servant.Checked.Exceptions (Throws)
import Servant ((:>))
import Servant.Server (Context(..))
import Servant.QuickCheck
  ( withServantServerAndContext,
    serverSatisfies,
    not500,
    defaultArgs,
    (<%>),
    Args (..),
  )
import Servant.QuickCheck.Internal.HasGenRequest (HasGenRequest(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "" $
  it "API demonstrates some best practices" $
    withServantServerAndContext api ctx (return server) $ \burl ->
      serverSatisfies api burl args (not500
                                 <%> mempty)

args :: Args
args = defaultArgs { maxSuccess = 500 }

ctx :: Context '[]
ctx = EmptyContext

instance (HasGenRequest rest) => HasGenRequest (Throws err :> rest) where
  genRequest _ = genRequest (Proxy :: Proxy rest)
