module Main (main) where

import JsonFeed
import System.FilePath ((</>), (<.>))
import Test.Hspec

import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Either as Either


main :: IO ()
main = hspec . parallel . describe "JsonFeed" $ do
  Monad.forM_ feeds $ \feed -> do
    it ("parses the " ++ feed ++ " feed") $ do
      let file = "feeds" </> feed <.> "json"
      contents <- ByteString.readFile file
      parseFeed contents `shouldSatisfy` Either.isRight

feeds :: [String]
feeds =
  [ "allenpike.com"
  , "daringfireball.net"
  , "flyingmeat.com"
  , "hypercritical.co"
  , "inessential.com"
  , "jsonfeed.org"
  , "manton.org"
  , "maybepizza.com"
  , "shapeof.com"
  , "therecord.co"
  , "timetable.manton.org"
  ]
