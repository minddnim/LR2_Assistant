import Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString.Char8 as B
import Control.Applicative

data MusicData = MusicData {
  dID :: Integer,
  dLevel :: String,
  dMusicTitle :: String,
  dBmsID :: Integer,
  dOrgArtist :: String,
  dOrgArtistURL :: String,
  dScoreSite :: String,
  dScoreSiteURL :: String,
  dComment :: String
} deriving (Show)

pMusicData :: Parser MusicData
pMusicData = do
  char '['
  iD <- B.unpack <$> pID
  char ','
  level <- B.unpack <$> pLevel
  char ','
  musicTitle <- B.unpack <$> pMusicTitle
  char ','
  bmsID <- B.unpack <$> pBmsID
  char ','
  (artUrl, artName) <- pOrgArtistInfo
  char ','
  (siteURL, siteName) <- pScoreSiteInfo
  char ','
  comment <- B.unpack <$> pComment
  char ','
  char ']'
  return $ MusicData (read iD) level musicTitle (read bmsID) artName artUrl siteName siteURL comment

pID :: Parser ByteString
pID = AP.takeTill $ inClass ","

pLevel :: Parser ByteString
pLevel = do 
  level <- pLevelHtmlTag <|> pLevelHtmlNoTag
  return level

pMusicTitle :: Parser ByteString
pMusicTitle = do
  musicTitle <- pGetInfo
  return musicTitle

pBmsID :: Parser ByteString
pBmsID = do
  bmsID <- pGetInfo
  return bmsID

pOrgArtistInfo :: Parser (String, String)
pOrgArtistInfo = do
  orgArtist <- pURLAndName <|> pNoURLAndName
  return orgArtist

pScoreSiteInfo :: Parser (String, String)
pScoreSiteInfo = do
  scoreSite <- pURLAndName <|> pNoURLAndName
  return scoreSite

pComment :: Parser ByteString
pComment = do
  comment <- pGetInfo
  return comment

pLevelHtmlTag :: Parser ByteString
pLevelHtmlTag = do
  char '"'
  pHtmlTag
  level <- AP.takeTill $ inClass "<"
  pHtmlTag
  char '"'
  return level

pLevelHtmlNoTag :: Parser ByteString
pLevelHtmlNoTag = do
  level <- pGetInfo
  return level

pURLAndName :: Parser (String, String)
pURLAndName = do
  char '"'
  char '<'
  AP.takeTill $ inClass "'"
  char '\''
  url <- AP.takeTill $ inClass "'"
  char '\''
  char '>'
  name <- AP.takeTill $ inClass "<"
  pHtmlTag
  char '"'
  return (B.unpack url, B.unpack name)

pNoURLAndName :: Parser (String, String)
pNoURLAndName = do
  name <- pGetInfo
  return ("", B.unpack name)

pHtmlTag :: Parser ByteString
pHtmlTag = do
  char '<'
  content <- AP.takeTill $ inClass ">"
  char '>'
  return content

pGetInfo :: Parser ByteString
pGetInfo = do
  char '"'
  info <- AP.takeTill $ inClass "\""
  char '"'
  return info

main :: IO ()
main = print $ parseOnly pMusicData (B.pack "[54,\"◎3\",\"祭月 (皿祭)\",\"132517\",\"<a href='http://flowermaster.web.fc2.com/'>わや</a>\",\"<a href='http://yaruki0.sakura.ne.jp/bms/ondanyugi4.html'>sa10</a>\",\"第六回差分企画 5KEYS\",]")

