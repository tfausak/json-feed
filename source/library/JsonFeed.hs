{-# LANGUAGE DeriveGeneric #-}

-- | <https://jsonfeed.org>
module JsonFeed
  ( parseFeed,
    renderFeed,

    -- * Types
    Feed (..),
    Author (..),
    Item (..),
    Attachment (..),
    Hub (..),

    -- * Wrappers
    Html (..),
    Mime (..),
    Url (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, Value)
import qualified Data.Aeson as Json (eitherDecode, encode)
import Data.Aeson.Types (Options)
import qualified Data.Aeson.Types as Json
import Data.ByteString.Lazy (ByteString)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Network.Mime (MimeType)
import Network.URI (URI)
import qualified Network.URI as Uri
import Numeric.Natural (Natural)
import Text.HTML.TagSoup (Tag)
import qualified Text.HTML.TagSoup as Html

parseFeed :: ByteString -> Either String Feed
parseFeed = Json.eitherDecode

renderFeed :: Feed -> ByteString
renderFeed = Json.encode

data Feed = Feed
  { -- | The feed author. The author object has several members. These are all
    -- optional --- but if you provide an author object, then at least one is
    -- required.
    feedAuthor :: Maybe Author,
    -- | Provides more detail, beyond the title, on what the feed is about. A
    -- feed reader may display this text.
    feedDescription :: Maybe Text,
    -- | Says whether or not the feed is finished --- that is, whether or not it
    -- will ever update again. A feed for a temporary event, such as an instance
    -- of the Olympics, could expire. If the value is 'True', then it's expired.
    -- Any other value, or the absence of 'feedExpired', means the feed may
    -- continue to update.
    feedExpired :: Maybe Bool,
    -- | The URL of an image for the feed suitable to be used in a source list.
    -- It should be square and relatively small, but not smaller than 64 x 64 (so
    -- that it can look good on retina displays). As with 'feedIcon', this image
    -- should use transparency where appropriate, since it may be rendered on a
    -- non-white background.
    feedFavicon :: Maybe Url,
    -- | The URL of the feed, and serves as the unique identifier for the feed.
    -- As with 'feedHomePageUrl', this should be considered required for feeds on
    -- the public web.
    feedFeedUrl :: Maybe Url,
    -- | The URL of the resource that the feed describes. This resource may or
    -- may not actually be a "home" page, but it should be an HTML page. If a
    -- feed is published on the public web, this should be considered as
    -- required. But it may not make sense in the case of a file created on a
    -- desktop computer, when that file is not shared or is shared only
    -- privately.
    feedHomePageUrl :: Maybe Url,
    -- | Describes endpoints that can be used to subscribe to real-time
    -- notifications from the publisher of this feed. Each object has a type and
    -- URL, both of which are required.
    feedHubs :: Maybe [Hub],
    -- | The URL of an image for the feed suitable to be used in a timeline, much
    -- the way an avatar might be used. It should be square and relatively large
    -- --- such as 512 x 512 --- so that it can be scaled-down and so that it can
    -- look good on retina displays. It should use transparency where
    -- appropriate, since it may be rendered on a non-white background.
    feedIcon :: Maybe Url,
    -- | An array of objects that describe each object in the list.
    feedItems :: [Item],
    -- | The URL of a feed that provides the next /n/ items, where /n/ is
    -- determined by the publisher. This allows for pagination, but with the
    -- expectation that reader software is not required to use it and probably
    -- won't use it very often. 'feedNextUrl' must not be the same as
    -- 'feedFeedUrl', and it must not be the same as a previous 'feedNextUrl' (to
    -- avoid infinite loops).
    feedNextUrl :: Maybe Url,
    -- | The name of the feed, which will often correspond to the name of the
    -- website (blog, for instance), though not necessarily.
    feedTitle :: Text,
    -- | A description of the purpose of the feed. This is for the use of people
    -- looking at the raw JSON, and should be ignored by feed readers.
    feedUserComment :: Maybe Text,
    -- | The URL of the version of the format the feed uses.
    feedVersion :: Url
  }
  deriving (Eq, Generic, Show)

instance FromJSON Feed where
  parseJSON = Json.genericParseJSON (jsonOptions "feed")

instance ToJSON Feed where
  toJSON = Json.genericToJSON (jsonOptions "feed")

data Author = Author
  { -- | The URL for an image for the author. As with icon, it should be square
    -- and relatively large --- such as 512 x 512 --- and should use transparency
    -- where appropriate, since it may be rendered on a non-white background.
    authorAvatar :: Maybe Url,
    -- | The author's name.
    authorName :: Maybe Text,
    -- | The URL of a site owned by the author. It could be a blog, micro-blog,
    -- Twitter account, and so on. Ideally the linked-to page provides a way to
    -- contact the author, but that's not required. The URL could be a @mailto:@
    -- link, though we suspect that will be rare.
    authorUrl :: Maybe Url
  }
  deriving (Eq, Generic, Show)

instance FromJSON Author where
  parseJSON value = do
    author <- Json.genericParseJSON (jsonOptions "author") value
    case (authorAvatar author, authorName author, authorUrl author) of
      (Nothing, Nothing, Nothing) -> fail ("invalid Author: " <> show author)
      _ -> pure author

instance ToJSON Author where
  toJSON = Json.genericToJSON (jsonOptions "author")

data Item = Item
  { -- | Lists related resources. Podcasts, for instance, would include an
    -- attachment that's an audio or video file.
    itemAttachments :: Maybe [Attachment],
    -- | Has the same structure as the top-level 'feedAuthor'. If not specified
    -- in an item, then the top-level author, if present, is the author of the
    -- item.
    itemAuthor :: Maybe Author,
    -- | The URL of an image to use as a banner. Some blogging systems (such as
    -- Medium) display a different banner image chosen to go with each post, but
    -- that image wouldn't otherwise appear in the content_html. A feed reader
    -- with a detail view may choose to show this banner image at the top of the
    -- detail view, possibly with the title overlaid.
    itemBannerImage :: Maybe Url,
    -- | 'itemContentHtml' and 'itemContentText' are each optional strings ---
    -- but one or both must be present. This is the HTML or plain text of the
    -- item. Important: the only place HTML is allowed in this format is in
    -- 'itemContentHtml'. A Twitter-like service might use 'itemContentText',
    -- while a blog might use 'itemContentHtml'. Use whichever makes sense for
    -- your resource. (It doesn't even have to be the same for each item in a
    -- feed.)
    itemContentHtml :: Maybe Html,
    -- | See 'itemContentHtml'.
    itemContentText :: Maybe Text,
    -- | Specifies the modification date in RFC 3339 format.
    itemDateModified :: Maybe UTCTime,
    -- | Specifies the date in RFC 3339 format. (Example:
    -- @2010-02-07T14:04:00-05:00@.)
    itemDatePublished :: Maybe UTCTime,
    -- | The URL of a page elsewhere. This is especially useful for linkblogs. If
    -- 'itemUrl' links to where you're talking about a thing, then
    -- 'itemExternalUrl' links to the thing you're talking about.
    itemExternalUrl :: Maybe Url,
    -- | Unique for the item in the feed over time. If an item is ever updated,
    -- the ID should be unchanged. New items should never use a previously-used
    -- ID. If an ID is presented as a number or other type, a JSON Feed reader
    -- must coerce it to a string. Ideally, the ID is the full URL of the
    -- resource described by the item, since URLs make great unique identifiers.
    itemId :: Value,
    -- | The URL of the main image for the item. This image may also appear in
    -- the 'itemContentHtml' --- if so, it's a hint to the feed reader that this
    -- is the main, featured image. Feed readers may use the image as a preview
    -- (probably resized as a thumbnail and placed in a timeline).
    itemImage :: Maybe Url,
    -- | A plain text sentence or two describing the item. This might be
    -- presented in a timeline, for instance, where a detail view would display
    -- all of 'itemContentHtml' or 'itemContentText'.
    itemSummary :: Maybe Text,
    -- | Can have any plain text values you want. Tags tend to be just one word,
    -- but they may be anything. Note: they are not the equivalent of Twitter
    -- hashtags. Some blogging systems and other feed formats call these
    -- categories.
    itemTags :: Maybe [Text],
    -- | Plain text. Microblog items in particular may omit titles.
    itemTitle :: Maybe Text,
    -- | The URL of the resource described by the item. It's the permalink. This
    -- may be the same as the ID --- but should be present regardless.
    itemUrl :: Maybe Url
  }
  deriving (Eq, Generic, Show)

instance FromJSON Item where
  parseJSON value = do
    item <- Json.genericParseJSON (jsonOptions "item") value
    case (itemContentHtml item, itemContentText item) of
      (Nothing, Nothing) -> fail ("invalid Item: " <> show item)
      _ -> pure item

instance ToJSON Item where
  toJSON = Json.genericToJSON (jsonOptions "item")

data Attachment = Attachment
  { -- | Specifies how long it takes to listen to or watch, when played at normal
    -- speed.
    attachmentDurationInSeconds :: Maybe Natural,
    -- | Specifies the type of the attachment, such as @audio/mpeg@.
    attachmentMimeType :: Mime,
    -- | Specifies how large the file is.
    attachmentSizeInBytes :: Maybe Natural,
    -- | Is a name for the attachment. Important: if there are multiple
    -- attachments, and two or more have the exact same title (when title is
    -- present), then they are considered as alternate representations of the
    -- same thing. In this way a podcaster, for instance, might provide an audio
    -- recording in different formats.
    attachmentTitle :: Maybe Text,
    -- | Specifies the location of the attachment.
    attachmentUrl :: Url
  }
  deriving (Eq, Generic, Show)

instance FromJSON Attachment where
  parseJSON = Json.genericParseJSON (jsonOptions "attachment")

instance ToJSON Attachment where
  toJSON = Json.genericToJSON (jsonOptions "attachment")

data Hub = Hub
  { hubType :: Text,
    hubUrl :: Url
  }
  deriving (Eq, Generic, Show)

instance FromJSON Hub where
  parseJSON = Json.genericParseJSON (jsonOptions "hub")

instance ToJSON Hub where
  toJSON = Json.genericToJSON (jsonOptions "hub")

newtype Html = Html
  { htmlValue :: [Tag Text]
  }
  deriving (Eq, Show)

instance FromJSON Html where
  parseJSON =
    Json.withText
      "Html"
      (\text -> pure Html {htmlValue = Html.parseTags text})

instance ToJSON Html where
  toJSON html = Json.String (Html.renderTags (htmlValue html))

newtype Mime = Mime
  { mimeValue :: MimeType
  }
  deriving (Eq, Show)

instance FromJSON Mime where
  parseJSON =
    Json.withText
      "Mime"
      (\text -> pure Mime {mimeValue = Text.encodeUtf8 text})

instance ToJSON Mime where
  toJSON mime = Json.String (Text.decodeUtf8 (mimeValue mime))

newtype Url = Url
  { urlValue :: URI
  }
  deriving (Eq, Show)

instance FromJSON Url where
  parseJSON =
    Json.withText
      "Url"
      ( \text -> case Uri.parseURI (Text.unpack text) of
          Just uri -> pure Url {urlValue = uri}
          Nothing -> fail ("invalid Url: " <> show text)
      )

instance ToJSON Url where
  toJSON url = Json.String (Text.pack (show (urlValue url)))

jsonOptions :: String -> Options
jsonOptions prefix =
  Json.defaultOptions {Json.fieldLabelModifier = fieldLabelModifier prefix}

fieldLabelModifier :: String -> String -> String
fieldLabelModifier prefix string =
  Json.camelTo2 '_' (unsafeDropPrefix prefix string)

unsafeDropPrefix :: String -> String -> String
unsafeDropPrefix prefix string = case dropPrefix prefix string of
  Just suffix -> suffix
  Nothing ->
    error
      ( unwords
          ["unsafeDropPrefix:", show prefix, "is not a prefix of", show string]
      )

dropPrefix :: String -> String -> Maybe String
dropPrefix prefix string =
  if prefix `List.isPrefixOf` string
    then Just (drop (length prefix) string)
    else Nothing
