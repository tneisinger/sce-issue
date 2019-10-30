{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (api, server)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Servant.Server (Context(..))
import Servant.QuickCheck
  ( withServantServerAndContext,
    serverSatisfies,
    not500,
    defaultArgs,
    (<%>),
    Args (..),
  )

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
