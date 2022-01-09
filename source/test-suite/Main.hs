module Main
  ( main
  ) where

import System.FilePath ((<.>), (</>))

import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Either as Either
import qualified JsonFeed
import qualified Test.Hspec as Hspec


main :: IO ()
main = Hspec.hspec . Hspec.parallel . Hspec.describe "JsonFeed" $ do
  Monad.forM_ feeds $ \feed -> do
    Hspec.it ("parses the " <> feed <> " feed") $ do
      let file = "feeds" </> feed <.> "json"
      contents <- ByteString.readFile file
      JsonFeed.parseFeed contents `Hspec.shouldSatisfy` Either.isRight

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
