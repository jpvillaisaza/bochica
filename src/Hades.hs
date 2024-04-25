module Hades where

-- base
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Version (showVersion)
import System.Exit (die)

-- bytestring
import Data.ByteString.Lazy (ByteString)

-- hades
import Paths_hades (version)

-- http-client
import Network.HTTP.Client (httpLbs, requestFromURI, responseBody)

-- http-client-tls
import Network.HTTP.Client.TLS (newTlsManager)

-- network-uri
import Network.URI (URI, parseURI, parseURIReference, uriScheme)

-- optparse-applicative
import Options.Applicative

-- tagsoup
import Text.HTML.TagSoup (Tag, fromAttrib, isTagOpenName, parseTags)
import Text.HTML.TagSoup.Match (getTagContent)

-- utf8-string
import qualified Data.ByteString.Lazy.UTF8 as LBSU8 (toString)


main :: IO ()
main = do
  opt <- execParser optParserInfo
  eFeedLinks <- fetchAndDiscoverFeedLinks (optUrl opt)
  case eFeedLinks of
    Left err ->
      die err
    Right feedLinks ->
      for_ feedLinks $ \feedLink -> do
        putStr (fromMaybe "No title" (mTitle feedLink))
        putStrLn (" (" <> show (feedType feedLink) <> "): ")
        print (feedUrl feedLink)

data Opt = Opt
  { optUrl :: URI
  }

optParser :: Parser Opt
optParser =
  Opt <$> urlParser

urlParser :: Parser URI
urlParser =
  argument (eitherReader urlReader)
    (metavar "URL" <> help "URL")
  where
    urlReader urlStr =
      case parseURI urlStr of
        Just url | uriScheme url `elem` ["http:", "https:"] -> do
          Right url
        Just _ ->
          Left "Invalid scheme"
        Nothing ->
          Left "Invalid URL"

optParserInfo :: ParserInfo Opt
optParserInfo =
  info (optParser <**> helper <**> versioner)
    (fullDesc
      <> progDesc ""
      <> header "hades"
      <> footer "")
  where
    versioner =
      simpleVersioner ("hades " <> showVersion version)

fetchAndDiscoverFeedLinks :: URI -> IO (Either String [FeedLink])
fetchAndDiscoverFeedLinks htmlUrl = do
  request <- requestFromURI htmlUrl
  manager <- newTlsManager
  response <- httpLbs request manager
  pure (Right (discoverFeedLinksInHtml (responseBody response)))

discoverFeedLinksInHtml :: ByteString -> [FeedLink]
discoverFeedLinksInHtml =
  mapMaybe parseFeedLink
    . getTagContent "head" (const True)
    . parseTags
    . LBSU8.toString

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
