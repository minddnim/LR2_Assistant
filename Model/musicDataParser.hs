module MusicDataParser
(
  MusicData(..),
  getMusicDatas
) where

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

getMusicDatas :: String -> [MusicData]
getMusicDatas str =
  case parseOnly pMusicDatas byteStr of
    Left _ -> error "parse error."
    Right results -> results
  where byteStr = B.pack str

pMusicDatas :: Parser [MusicData]
pMusicDatas = do
  string $ B.pack "var"
  many space
  string $ B.pack "mname"
  many space
  char '='
  many space
  char '['
  many pMusicData
--  musicDatas <- many pMusicData
--  return musicDatas

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
  char ','
  many space
  return $ MusicData (read iD) level musicTitle (read bmsID) artName artUrl siteName siteURL comment

pID :: Parser ByteString
pID = AP.takeTill $ inClass ","

pLevel :: Parser ByteString
pLevel = pLevelHtmlTag <|> pLevelHtmlNoTag

pMusicTitle :: Parser ByteString
pMusicTitle = pGetInfo

pBmsID :: Parser ByteString
pBmsID = pGetInfo

pOrgArtistInfo :: Parser (String, String)
pOrgArtistInfo = pURLAndName <|> pNoURLAndName

pScoreSiteInfo :: Parser (String, String)
pScoreSiteInfo = pURLAndName <|> pNoURLAndName

pComment :: Parser ByteString
pComment = pGetInfo

pLevelHtmlTag :: Parser ByteString
pLevelHtmlTag = do
  char '"'
  pHtmlTag
  level <- AP.takeTill $ inClass "<"
  pHtmlTag
  char '"'
  return level

pLevelHtmlNoTag :: Parser ByteString
pLevelHtmlNoTag = pGetInfo

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
