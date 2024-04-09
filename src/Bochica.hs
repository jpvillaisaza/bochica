module Bochica where

-- base
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import System.Environment (getArgs)
import System.Exit (die)

-- http-client
import Network.HTTP.Client (httpLbs, requestFromURI, responseBody)

-- http-client-tls
import Network.HTTP.Client.TLS (newTlsManager)

-- network-uri
import Network.URI (URI, parseURI, parseURIReference, uriScheme)

-- tagsoup
import Text.HTML.TagSoup (Tag, fromAttrib, isTagOpenName, parseTags)

-- utf8-string
import qualified Data.ByteString.Lazy.UTF8 as LBSU8 (toString)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [htmlUrl] -> do
      eFeedLinks <- discoverFeedLinks htmlUrl
      case eFeedLinks of
        Left err ->
          die err
        Right feedLinks ->
          for_ feedLinks $ \feedLink -> do
            putStr (fromMaybe "No title" (mTitle feedLink))
            putStrLn (" (" <> show (feedType feedLink) <> "): ")
            print (feedUrl feedLink)
    _ ->
      die "usage: bochica <html-url>"

discoverFeedLinks :: String -> IO (Either String [FeedLink])
discoverFeedLinks htmlUrlStr =
  case parseURI htmlUrlStr of
    Just htmlUrl | uriScheme htmlUrl `elem` ["http:", "https:"] -> do
      request <- requestFromURI htmlUrl
      manager <- newTlsManager
      response <- httpLbs request manager
      let tags = parseTags (LBSU8.toString (responseBody response))
      pure (Right (mapMaybe parseFeedLink tags))
    Just _ ->
      pure (Left "Invalid scheme")
    Nothing ->
      pure (Left "Invalid URL")

data FeedType
  = Atom
  | JsonFeed
  | Rss
  deriving (Show)

data FeedLink = FeedLink
  { feedType :: FeedType
  , feedUrl :: URI
  , mTitle :: Maybe String
  }
  deriving (Show)

parseFeedLink :: Tag String -> Maybe FeedLink
parseFeedLink tag =
  if isFeed
    then
      case parseURIReference (fromAttrib "href" tag) of
        Just feedUrl ->
          Just FeedLink
            { feedType = fromMaybe Rss mFeedType
            , feedUrl
            , mTitle
            }
        Nothing ->
          Nothing
    else
      Nothing
  where
    isFeed =
      isTagOpenName "link" tag
        && fromAttrib "rel" tag == "alternate"
        && isJust mFeedType
    mTitle =
      case fromAttrib "title" tag of
        "" ->
          Nothing
        title ->
          Just title
    mFeedType =
      case fromAttrib "type" tag of
        "application/atom+xml" ->
          Just Atom
        "application/json+feed" ->
          Just JsonFeed
        "application/json" ->
          Just JsonFeed
        "application/rss+xml" ->
          Just Rss
        _ ->
          Nothing
